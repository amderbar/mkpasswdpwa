module Main.CLI (main) where

import Prelude

import ArgParse.Basic (ArgParser, argument, choose, default, flagHelp, fromRecord, int, parseArgs, printArgError, separated, unfolded1, unformat)
import Data.Array (drop)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Char.Subset (SymbolChar, hiragana, symbols)
import Data.Char.Subset (fromString) as SubsetChar
import Data.Count (Count, toCountE)
import Data.Either (Either(..), note)
import Data.Int (fromString) as Int
import Data.Length (Length, toLengthE)
import Data.Passwd.Gen (genPasswd)
import Data.Policy (CharGenSrc(..), Policy, defaultPolicy)
import Data.String (Pattern(..))
import Data.String.NonEmpty (fromString) as NES
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Process (argv) as Process
import Test.QuickCheck.Gen (randomSampleOne)

main :: Effect Unit
main = do
  let
    execParser =
      argParser
        { header: "mkpasswdpwa"
        , desc: "Random string generation"
        , parser: genPasswd <$> policyArg <* flagHelp
        }
  args <- Process.argv <#> drop 2
  case execParser args of
    Right gen -> logShow =<< randomSampleOne gen
    Left err -> log (printArgError err)
  where
  argParser { header, desc, parser } = parseArgs header desc parser

policyArg :: ArgParser Policy
policyArg =
  let
    digitNumArg =
      argument [ "--digit", "-d" ] "Minimum number of digits to include."
        # int
        # countArg

    lowercaseNum =
      argument [ "--lowercase", "-c" ] "Minimum number of lowercase characters to include."
        # int
        # countArg

    capitalNum =
      argument [ "--capital", "-C" ] "Minimum number of capital letters to include."
        # int
        # countArg

    symbolCharType =
      argument [ "--symbol", "-s" ] "Minimum number of symbols to include."
        # separated "INTEGER [SYMBOLS]" (Pattern " ")
        # unformat "INTEGER and [SYMBOLS]" case _ of
          [cnt] -> do
            c <- cnt # strToCountE
            pure { count: c, genSrc: Symbols symbols }
          [cnt, str] -> do
            c <- cnt # strToCountE
            s <- str # (SubsetChar.fromString (show >>> append "Invalid Symbol:") >=> fromArray >>> note "Expected NonEmpty SYMBOLS")
            pure { count: c, genSrc: Symbols (s :: NonEmptyArray SymbolChar) }
          _ -> Left "Expected INTEGER and SYMBOLS"

    hiraganaNum =
      argument [ "--hiragana" ] "Minimum number of hiragana to include."
        # int
        # countArg

    customCharType =
      argument [ "--custom" ] "Custom Charactor set."
        # separated "INTEGER STRING" (Pattern " ")
        # unformat "INTEGER and NonEmpty STRING" case _ of
          [cnt, str] -> do
            c <- cnt # strToCountE
            s <- str # NES.fromString >>> note "Expected NonEmpty STRING"
            pure { count: c, genSrc: AnyChars "" s }
          _ -> Left "Expected INTEGER and NonEmpty STRING"

    charTypeConf =
      choose "required charactor types"
        [ digitNumArg <#> { count: _, genSrc: Digits }
        , lowercaseNum <#> { count: _, genSrc: LowercaseAlphabets }
        , capitalNum <#> { count: _, genSrc: UppercaseAlphabets }
        , symbolCharType
        , hiraganaNum <#> { count: _, genSrc: Hiraganas hiragana }
        , customCharType
        ]
  in
    fromRecord
      { length:
          argument [ "--length", "-l" ] "Required length."
            # int
            # lengthArg
            # default defaultPolicy.length
      , required:
          unfolded1 charTypeConf
            # default defaultPolicy.required
      }

lengthArg :: ArgParser Int -> ArgParser Length
lengthArg = unformat "INTEGER" (toLengthE boundedIntErrorMsg)

countArg :: ArgParser Int -> ArgParser Count
countArg = unformat "INTEGER" (toCountE boundedIntErrorMsg)

strToCountE :: String -> Either String Count
strToCountE = Int.fromString >>> note "Expected INTEGER" >=> toCountE boundedIntErrorMsg

boundedIntErrorMsg :: Int -> Int -> String
boundedIntErrorMsg b t = "Expected INTEGER between " <> (show b) <> " to " <> (show t)
