-- |
-- This module provides a pager or cat utility.
-- The text is broken into pages based on the terminal size.
-- Forward and backward scrolling are supported.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}


module HCat where

import System.IO
import System.Info
import qualified System.Process as Process
import qualified System.IO.Error as IOError
import qualified System.Environment as Env
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Control.Exception as Exception
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified System.Directory as Directory
import qualified Text.Printf as Printf


-- |
-- The top-level function.
runHCat :: IO ()
runHCat = do
    targetFilePath <- eitherToErr =<< handleArgs
    contents <- TextIO.hGetContents =<< openFile targetFilePath ReadMode
    termSize <- getTerminalSize
    hSetBuffering stdout NoBuffering
    finfo <- fileInfo targetFilePath
    let pages = paginate termSize finfo contents
    showPages 0 pages


-- |
-- Takes an Either and throws an exception in error case (Left).
-- Otherwise returns the Right value.
eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e


-- |
-- Parses and validates the command line arguments.
handleArgs :: IO (Either String FilePath)
handleArgs =
    parseArgs <$> Env.getArgs
    where
        parseArgs argumentList =
            case argumentList of
                [] -> Left "No arguments provided!"
                [fname] -> Right fname
                _   -> Left "Multiple files not supported."


-- |
-- The commands supported by the pager.
data ContinueCancel = Help | Previous | Continue | Cancel deriving (Eq, Show)


-- | Clear terminal.
clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"


-- |
-- Switch video to reverse mode.
reverseVideo :: IO ()
reverseVideo = BS.putStr "\^[[7m"


-- |
-- Reset video to normal mode.
resetVideo :: IO ()
resetVideo = BS.putStr "\^[[0m"


-- |
-- Get keys and map to commands.
getContinue :: IO ContinueCancel
getContinue = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    c <- hGetChar stdin
    case c of
        'h' -> return Help
        ' ' -> return Continue
        'n' -> return Continue
        'p' -> return Previous
        'q' -> return Cancel
        _   -> getContinue


-- |
-- Show a simple help screen.
showHelp:: Int -> [Text.Text] -> IO ()
showHelp page pages = do
    clearScreen
    reverseVideo
    putStrLn "=== HCat Help ==="
    resetVideo
    putStrLn ""
    putStrLn "usage: hcat <filepath>"
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "h - show this help"
    putStrLn "<space> - next page"
    putStrLn "n - next page"
    putStrLn "p - previous page"
    putStrLn "q - quit"
    putStrLn ""
    putStrLn "Press any key to return to file view."
    hGetChar stdin
    showPages page pages


-- | Show and navigate pages.
showPages :: Int -> [Text.Text] -> IO ()
showPages _ [] = return ()
showPages page pages = do
    clearScreen
    TextIO.putStrLn $ pages !! page
    cont <- getContinue
    case cont of
        Help -> showHelp page pages
        Previous -> showPages (if page > 1 then (page - 1) else 0) pages
        Continue -> showPages (page + 1) pages
        Cancel  -> return ()


-- |
-- Group text into pages.
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems =
    let (hd, tl) = splitAt n elems
    in hd : groupsOf n tl


-- |
-- Wrap a line between words if possible.
wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
    | Text.length lineText <= lineLength = [lineText]
    | otherwise =
        let
          (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
        in firstLine : wordWrap lineLength (overflow <> nextLines)
    where
      softWrap hardwrappedText textIndex
        | textIndex <= 0 = (hardwrappedText, Text.empty)
        | Text.index hardwrappedText textIndex == ' ' =
            let (wrappedLine, rest) =  Text.splitAt textIndex hardwrappedText
            in (wrappedLine, Text.tail rest)
        | otherwise = softWrap hardwrappedText (textIndex -1)


-- |
-- Record for screen dimensions.
data ScreenDimensions = ScreenDimensions {
    screenRows :: Int,
    screenColumns :: Int
  } deriving Show


-- |
-- Perform pagination of the text and add status line.
-- Take care of wrapping lines longer than the width of the terminal.
paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
    let
        rows' = rows - 2  -- consider status line and empty line at bottom
        wrappedLines = concatMap (wordWrap cols) (Text.lines text)
        pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
        pageCount = length pages
        statusLines = map (formatFileInfo finfo cols pageCount) [1..pageCount]
    in zipWith (<>) pages statusLines
    where
        padTo :: Int -> [Text.Text] -> [Text.Text]
        padTo lineCount rowsToPad =
            take lineCount $ rowsToPad <> repeat ""


{- |
Determine the size of the terminal.
This code depends on the OS and a tool named "tput".

>>> getTerminalSize
ScreenDimensions {screenRows = 67, screenColumns = 103}
-}
getTerminalSize :: IO ScreenDimensions
getTerminalSize =
    case System.Info.os of
        "darwin" -> tputScreenDimensions
        "linux"  -> tputScreenDimensions
        _other   -> pure $ ScreenDimensions 25 80
    where
        tputScreenDimensions :: IO ScreenDimensions
        tputScreenDimensions = do
            lines <- Process.readProcess "tput" ["lines"] ""
            cols  <- Process.readProcess "tput" ["cols"] ""
            let lines' = read $ init lines
                cols'  = read $ init cols
            return $ ScreenDimensions lines' cols'


-- |
-- Record holding the information required for the status line.
data FileInfo = FileInfo {
    filePath :: FilePath,       -- ^ path to the file
    fileSize :: Int,            -- ^ file size in bytes
    fileMTime :: Clock.UTCTime, -- ^ last modification time
    fileReadable :: Bool,       -- ^ is file readable
    fileWriteable :: Bool,      -- ^ is file writeable
    fileExecutable :: Bool      -- ^ is file executable
} deriving Show


{- |
Determine relevant information of the file to view.

>>>  fileInfo "src/HCat.hs"
FileInfo {filePath = "src/HCat.hs", fileSize = 8450, fileMTime = 2022-05-21 15:18:59.607415167 UTC, fileReadable = True, fileWriteable = True, fileExecutable = False}
-}
fileInfo :: FilePath -> IO  FileInfo
fileInfo filePath = do
    perms <- Directory.getPermissions filePath
    mtime <- Directory.getModificationTime filePath
    size <- Text.length <$> TextIO.readFile filePath
    return FileInfo {
        filePath = filePath,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable perms,
        fileWriteable = Directory.writable perms,
        fileExecutable = Directory.executable perms
    }


{- |
Format the status line.
Consider the case of terminal width less than the length of the status line.

In the first example maxWidth is set to 80 and the status line is truncated after the timestamp.
The trailing ... make this clear.

>>> finfo <- fileInfo "src/HCat.hs"
>>> formatFileInfo finfo 80 2 1
"\ESC[7msrc/HCat.hs | permissions: rw- | 8450 bytes | modified: 2022-05-21 15:18:59 |...\ESC[0m"

After increasing the linelength to 90 the complete status line can be rendered.

>>> formatFileInfo finfo 90 2 1
"\ESC[7msrc/HCat.hs | permissions: rw- | 8450 bytes | modified: 2022-05-21 15:18:59 | page: 1 of 2\ESC[0m"
-}
formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
    let
        statusLine = Text.pack $
            Printf.printf
                "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
                filePath
                permissionString
                fileSize
                timestamp
                currentPage
                totalPages
        permissionString = [ if fileReadable then 'r' else '-',
                             if fileWriteable then 'w' else '-',
                             if fileExecutable then 'x' else '-' ]
        timestamp = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
    in invertText (truncateStatus statusLine)
    where
        invertText inputStr =
            let
                reverseVideo = "\^[[7m"
                resetVideo = "\^[[0m"
            in reverseVideo <> inputStr <> resetVideo
        truncateStatus statusLine
          | maxWidth <= 3 = ""
          | Text.length statusLine > maxWidth =
                Text.take (maxWidth - 3) statusLine <> "..."
          | otherwise = statusLine

