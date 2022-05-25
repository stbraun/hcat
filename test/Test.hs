import qualified Data.Text as Text
import qualified Data.Time.Clock as Clock
import Data.Time.Calendar.OrdinalDate as OD

import Test.Hspec

import Specs

main :: IO ()
main = hspec $ do
    specHCat

