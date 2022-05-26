module Specs (
          specHCat
        )
    where

import qualified Data.Text as Text
import qualified Data.Time.Clock as Clock
import Data.Time.Calendar.OrdinalDate as OD

import Test.Hspec

import HCat


-- |
-- Test specifications for HCat module.
specHCat :: Spec
specHCat = do
    describe "paginate" $ do
        -- Consider 2 lines for status line.
        let
            text = Text.pack "line1\nline2\nline3\nline4\nline5\nline6\nline7"
        it "shall create 4 pages for a page length of 2 (screenRows-2)" $
            (length $ paginate ScreenDimensions {screenRows=(2+2), screenColumns=40} fInfo text)
                `shouldBe` 4
        it "shall create 1 page for a page length of 7." $
            (length $ paginate ScreenDimensions {screenRows=(7+2), screenColumns=40} fInfo text)
                `shouldBe` 1
        it "shall create 1 page for a page length of 7 and 5 columns." $
            (length $ paginate ScreenDimensions {screenRows=(7+2), screenColumns=5} fInfo text)
                `shouldBe` 1
        it "shall create 2 pages for a page length of 7 and 4 columns." $
            (length $ paginate ScreenDimensions {screenRows=(7+2), screenColumns=4} fInfo text)
                `shouldBe` 2
    describe "formatFileInfo" $ do
        -- Consider reverseVideo and resetVideo sequence (4 chars) at start resp. end of text!
        it "shall not truncate statusline if screen is wide enough." $
            (formatFileInfo fInfo 120 1 1) `shouldNotSatisfy`
                (contains "...")
        it "shall truncate statusline if screen is not wide enough." $
            (formatFileInfo fInfo 20 1 1) `shouldSatisfy`
                (contains "...")
        it "shall insert the page and number of pages." $
            (Text.take 7 $ Text.takeEnd (7 + 4) $ formatFileInfo fInfo 120 23 1)
                `shouldBe` (Text.pack "1 of 23")
        it "shall contain the file path." $
            (formatFileInfo fInfo 120 23 1) `shouldSatisfy`
                (contains $ filePath fInfo)
        it "shall contain the file size." $
            (formatFileInfo fInfo 120 23 1) `shouldSatisfy`
                (contains $ show $ fileSize fInfo)
    describe "wordWrap" $ do
        it "shall wrap the line at the last possible word." $
            (wordWrap 11 (Text.pack "This is a line.") !! 1)
                `shouldBe` (Text.pack "line.")
        it "shall not wrap if the line is longer than the text." $
            (length $ wordWrap 16 (Text.pack "This is a line."))
                `shouldBe` 1
        it "shall not wrap if the line is as long as the text." $
            (length $ wordWrap 15 (Text.pack "This is a line."))
                `shouldBe` 1
        it "shall hard wrap if the line is shorter than the first word of the text." $
            (wordWrap 11 (Text.pack "This-is-a-line.") !! 1)
                `shouldBe` (Text.pack "ine.")


-- |
-- Checks whether string is contained in text.
contains :: String -> Text.Text -> Bool
contains subString text = Text.isInfixOf (Text.pack subString) text


-- |
-- Provide a FileInfo record for the tests.
fInfo :: FileInfo
fInfo = FileInfo { filePath="myFile.txt"
                 , fileSize=42
                 , fileMTime=Clock.UTCTime{Clock.utctDay=(OD.fromOrdinalDate 2022 111),
                                           Clock.utctDayTime=7200}
                 , fileReadable=True
                 , fileWriteable=False
                 , fileExecutable=False}

