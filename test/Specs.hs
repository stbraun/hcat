module Specs where

import qualified Data.Text as Text
import qualified Data.Time.Clock as Clock
import Data.Time.Calendar.OrdinalDate as OD

import Test.Hspec

import HCat


specHCat :: Spec
specHCat = do
    describe "paginate" $ do
        let
            text = Text.pack "line1\nline2\nline3\nline4\nline5\nline6\nline7"
        it "shall create 4 pages for a page length of 2 (screenRows-2)" $
            (length $ paginate ScreenDimensions {screenRows=4, screenColumns=40} fInfo text)
                `shouldBe` 4
        it "shall create 1 page for a page length of 7." $
            (length $ paginate ScreenDimensions {screenRows=9, screenColumns=40} fInfo text)
                `shouldBe` 1
        it "shall create 1 page for a page length of 7 and 5 columns." $
            (length $ paginate ScreenDimensions {screenRows=9, screenColumns=5} fInfo text)
                `shouldBe` 1
        it "shall create 2 pages for a page length of 7 and 4 columns." $
            (length $ paginate ScreenDimensions {screenRows=9, screenColumns=4} fInfo text)
                `shouldBe` 2
    describe "formatFileInfo" $ do
        -- Consider reverseVideo and resetVideo sequence (4 chars) at start resp. end of text!
        it "shall not truncate statusline if screen is wide enough." $
            (Text.isInfixOf (Text.pack "...") $ formatFileInfo fInfo 120 1 1)
                `shouldBe` False
        it "shall truncate statusline if screen is not wide enough." $
            (Text.isInfixOf (Text.pack "...") $ formatFileInfo fInfo 20 1 1)
                `shouldBe` True
        it "shall insert the page and number of pages." $
            (Text.take 7 $ Text.takeEnd (7 + 4) $ formatFileInfo fInfo 120 23 1)
                `shouldBe` (Text.pack "1 of 23")
        it "shall contain the file path." $
            (Text.isInfixOf (Text.pack $ filePath fInfo) $ formatFileInfo fInfo 120 23 1)
                `shouldBe` True
        it "shall contain the file size." $
            (Text.isInfixOf (Text.pack $ show $ fileSize fInfo) $ formatFileInfo fInfo 120 23 1)
                `shouldBe` True

fInfo :: FileInfo
fInfo = FileInfo { filePath="myFile.txt"
                 , fileSize=42
                 , fileMTime=Clock.UTCTime{Clock.utctDay=(OD.fromOrdinalDate 2022 111),
                                           Clock.utctDayTime=7200}
                 , fileReadable=True
                 , fileWriteable=False
                 , fileExecutable=False}

