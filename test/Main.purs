module Test.Main (main) where

import Prelude

import Data.Array.NonEmpty (head, singleton)
import Data.Char.GenSource (GenSrcExists, digits, lowercases, members, mkGenSource, uppercases)
import Data.Char.Subset (symbols)
import Data.Count (fromCount)
import Data.Foldable (elem, sum)
import Data.Length (fromLength)
import Data.Passwd (Passwd(..))
import Data.Passwd.Gen (genPasswd)
import Data.Policy (Policy, CharTypeConf)
import Data.String (length)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck (Result, arbitrary, (>=?))
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
            quickCheck $ passwdProp digits chkLength
          it "should contain more digits than specified in the policy" do
            quickCheck $ passwdProp digits \{required: r} -> chkCharTypeNum (head r)
          it "should contain more capital letters than specified in the policy" do
            quickCheck $ passwdProp uppercases \{required: r} -> chkCharTypeNum (head r)
          it "should contain more lowercase letters than specified in the policy" do
            quickCheck $ passwdProp lowercases \{required: r} -> chkCharTypeNum (head r)
          it "should contain more symbols than specified in the policy" do
            quickCheck $ passwdProp (mkGenSource symbols) \{required: r} -> chkCharTypeNum (head r)

passwdProp :: GenSrcExists -> (Policy -> Passwd -> Result) -> Gen Result
passwdProp src chk = do
  c <- arbitrary <#> { count: _, genSrc: src}
  p <- arbitrary <#> { length: _, required: singleton c}
  chk p <$> genPasswd p

chkLength :: Policy -> Passwd -> Result
chkLength p (Passwd r) = length r >=? fromLength p.length

chkCharTypeNum :: CharTypeConf -> Passwd -> Result
chkCharTypeNum {count, genSrc} (Passwd p) = countUp p >=? fromCount count
  where
  countUp :: String -> Int
  countUp =
    let charset = members genSrc
    in
      toCharArray
        >>> map (\c -> if c `elem` charset then 1 else 0)
        >>> sum
