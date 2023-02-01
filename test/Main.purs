module Test.Main (main) where

import Prelude

import Data.Array.NonEmpty (singleton)
import Data.Char.GenSource (digits, lowercases, members, mkGenSource, uppercases)
import Data.Char.Subset (hiragana, symbols)
import Data.Count (fromCount)
import Data.Foldable (elem, sum)
import Data.Length (fromLength)
import Data.Passwd (Passwd(..))
import Data.Passwd.Gen (genPasswd)
import Data.Policy (CharTypeConf)
import Data.String (length) as Str
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.QuickCheck (arbitrary, (>=?))
import Test.Spec (SpecT, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ (runSpec [ consoleReporter ] spec)

spec :: forall m. Monad m => SpecT Aff Unit m Unit
spec = do
  describe "Generated Passwd from Policy" do

    it "should be longer than the length specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: digits }
        p <- arbitrary <#> { length: _, required: singleton c }
        (Passwd r) <- genPasswd p
        pure $ Str.length r >=? fromLength p.length

    it "should contain more digits than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: digits }
        p <- arbitrary <#> { length: _, required: singleton c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more capital letters than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: uppercases }
        p <- arbitrary <#> { length: _, required: singleton c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more lowercase letters than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: lowercases }
        p <- arbitrary <#> { length: _, required: singleton c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more symbols than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: mkGenSource symbols }
        p <- arbitrary <#> { length: _, required: singleton c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more hiragana than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: mkGenSource hiragana }
        p <- arbitrary <#> { length: _, required: singleton c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

countCharType :: CharTypeConf -> String -> Int
countCharType {genSrc} =
  let charset = members genSrc
  in
    toCharArray
      >>> map (\c -> if c `elem` charset then 1 else 0)
      >>> sum
