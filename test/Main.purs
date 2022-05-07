module Test.Main (main) where

import Prelude
import Data.Char.Symbols.Gen (symbols)
import Data.Count (Count, fromCount)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, elem, sum)
import Data.Length (fromLength)
import Data.Maybe (Maybe(..))
import Data.Passwd (Passwd(..))
import Data.Passwd.Gen (genPasswd)
import Data.Policy (Policy)
import Data.String (length)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck (Result, arbitrary, (<?>), (===), (>=?))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Generated Passwd from Policy" do
          it "should be longer than the length specified in the policy" do
            quickCheck $ passwdProp chkLength
          it "should contain more digits than specified in the policy" do
            quickCheck $ passwdProp \p -> chkCharTypeNum (toCharArray "0123456789") p.digitNum
          it "should contain more capital letters than specified in the policy" do
            quickCheck $ passwdProp \p -> chkCharTypeNum (toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ") p.capitalNum
          it "should contain more lowercase letters than specified in the policy" do
            quickCheck $ passwdProp \p -> chkCharTypeNum (toCharArray "abcdefghijklmnopqrstuvwxyz") p.lowercaseNum
          it "should contain more symbols than specified in the policy" do
            quickCheck $ passwdProp \p -> chkCharTypeNum symbols p.symbolNum

passwdProp :: (Policy -> Passwd -> Result) -> Gen Result
passwdProp chk = do
  p <- arbitrary
  case genPasswd p of
    Right gen -> chk p <$> gen
    Left err -> pure (true <?> err)

chkLength :: Policy -> Passwd -> Result
chkLength p (Passwd r) = length r >=? fromLength p.length

chkCharTypeNum :: forall f. Foldable f => f Char -> Maybe Count -> Passwd -> Result
chkCharTypeNum charset mCnt (Passwd p) = case mCnt of
  Nothing -> countUp p === 0
  Just cnt -> countUp p >=? fromCount cnt
  where
  countUp :: String -> Int
  countUp =
    toCharArray
      >>> map (\c -> if c `elem` charset then 1 else 0)
      >>> sum
