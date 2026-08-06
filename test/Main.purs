module Test.Main (main) where

import Prelude

import Data.Char.Subset (hiragana, symbols)
import Data.Csv (encodeCsv, encodeField, formDataToRows)
import Data.GenSource (members)
import Data.Count (fromCount)
import Data.Foldable (elem, sum)
import Data.Length (fromLength)
import Data.Passwd (Passwd(..))
import Data.Passwd.Gen (genPasswd)
import Data.Policy (CharGenSrc(..), CharTypeConf)
import Data.String (length) as Str
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.QuickCheck (arbitrary, (>=?))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: forall m. Monad m => SpecT Aff Unit m Unit
spec = do
  describe "Generated Passwd from Policy" do

    it "should be longer than the length specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: Digits }
        p <- arbitrary <#> { length: _, required: pure c }
        (Passwd r) <- genPasswd p
        pure $ Str.length r >=? fromLength p.length

    it "should contain more digits than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: Digits }
        p <- arbitrary <#> { length: _, required: pure c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more capital letters than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: UppercaseAlphabets }
        p <- arbitrary <#> { length: _, required: pure c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more lowercase letters than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: LowercaseAlphabets }
        p <- arbitrary <#> { length: _, required: pure c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more symbols than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: Symbols symbols }
        p <- arbitrary <#> { length: _, required: pure c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

    it "should contain more hiragana than specified in the policy" do
      quickCheck do
        c <- arbitrary <#> { count: _, genSrc: Hiraganas hiragana }
        p <- arbitrary <#> { length: _, required: pure c }
        (Passwd r) <- genPasswd p
        pure $ countCharType c r >=? fromCount c.count

  describe "Data.Csv" do

    it "should quote fields containing commas" do
      encodeField "a,b" `shouldEqual` "\"a,b\""

    it "should escape double quotes" do
      encodeField "say \"hi\"" `shouldEqual` "\"say \"\"hi\"\"\""

    it "should quote fields containing newlines" do
      encodeField "a\nb" `shouldEqual` "\"a\nb\""

    it "should leave plain fields untouched" do
      encodeField "plain" `shouldEqual` "plain"

    it "should join rows with CRLF" do
      encodeCsv [ [ "a", "b" ], [ "c", "d" ] ] `shouldEqual` "a,b\r\nc,d"

    it "should prepend a header row to the form data" do
      formDataToRows [ { account: "foo", passwd: "bar", note: "baz" } ]
        `shouldEqual`
          [ [ "アカウント", "パスワード", "メモ" ], [ "foo", "bar", "baz" ] ]

countCharType :: CharTypeConf -> String -> Int
countCharType { genSrc } =
  let
    charset = members genSrc
  in
    toCharArray
      >>> map (\c -> if c `elem` charset then 1 else 0)
      >>> sum
