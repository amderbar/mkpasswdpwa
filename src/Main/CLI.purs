module Main.CLI (main) where

import Prelude

import ArgParse.Basic (ArgParser, argument, choose, default, flagHelp, fromRecord, int, parseArgs, printArgError, separated, unfolded1, unformat)
import Data.Array (drop, head, uncons)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Char.Subset (SymbolChar, hiragana, symbols)
import Data.Char.Subset (fromNonEmptyString) as SubsetChar
import Data.Count (Count, toCountE)
import Data.Either (Either(..), note)
import Data.Int (fromString) as Int
import Data.Length (Length, toLengthE)
import Data.Maybe (Maybe(..), maybe)
import Data.Passwd.Gen (genPasswd)
import Data.Policy (CharGenSrc(..), Policy, defaultPolicy)
import Data.String.NonEmpty (NonEmptyString, Pattern(..))
import Data.String.NonEmpty (fromString) as NES
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
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
    digitArg =
      argument [ "--digit" ] "Sampling from Digits"
        # int
        # count
        <#> { count: _, genSrc: Digits }

    uppercaseArg =
      argument [ "--uppercase" ] "Sampling from Cppercase letters"
        # int
        # count
        <#> { count: _, genSrc: UppercaseAlphabets }

    lowercaseArg =
      argument [ "--lowercase" ] "Sampling from Lowercase letters"
        # int
        # count
        <#> { count: _, genSrc: LowercaseAlphabets }

    symbolArg =
      argument [ "--symbol" ] "Sampling from Symbols"
        # separated "INTEGER [SYMBOLS]" (Pattern " ")
        # unformat "INTEGER and [SYMBOLS]" \arr -> case uncons arr of
            Just { head: cnt, tail: rest } -> do
              c <- cnt # strToCountE
              s <- (head rest) # maybe (pure symbols) (strToNonEmptyE >=> symbolCharsE)
              pure { count: c, genSrc: Symbols s }
            Nothing -> Left "Expected INTEGER and SYMBOLS"

    hiraganaArg =
      argument [ "--hiragana" ] "Sampling from Hiraganas"
        # int
        # count
        <#> { count: _, genSrc: Hiraganas hiragana }

    customArg =
      argument [ "--custom" ] "Sampling from Custom Charactor set."
        # separated "INTEGER STRING" (Pattern " ")
        # unformat "INTEGER and NonEmpty STRING" \arr -> case uncons arr of
            Just { head: cnt, tail: rest } | Just str <- head rest -> do
              c <- cnt # strToCountE
              s <- str # strToNonEmptyE <#> toNonEmptyCharArray
              pure { count: c, genSrc: AnyChars s }
            _ -> Left "Expected INTEGER and NonEmpty STRING"

    charTypeConfArg =
      choose "required charactor types"
        [ digitArg
        , uppercaseArg
        , lowercaseArg
        , symbolArg
        , hiraganaArg
        , customArg
        ]
  in
    fromRecord
      { length:
          argument [ "--length", "-l" ] "Required length."
            # int
            # length
            # default defaultPolicy.length
      , required:
          unfolded1 charTypeConfArg
            # default defaultPolicy.required
      }

length :: ArgParser Int -> ArgParser Length
length = unformat "LENGTH" (toLengthE boundedIntErrorMsg)

count :: ArgParser Int -> ArgParser Count
count = unformat "COUNT" (toCountE boundedIntErrorMsg)

strToCountE :: String -> Either String Count
strToCountE = Int.fromString >>> note "Expected INTEGER" >=> toCountE boundedIntErrorMsg

boundedIntErrorMsg :: Int -> Int -> String
boundedIntErrorMsg b t = "Expected INTEGER between " <> (show b) <> " to " <> (show t)

strToNonEmptyE :: String -> Either String NonEmptyString
strToNonEmptyE = NES.fromString >>> note "Expected NonEmpty STRING"

symbolCharsE :: NonEmptyString -> Either String (NonEmptyArray SymbolChar)
symbolCharsE = SubsetChar.fromNonEmptyString (show >>> append "Invalid Symbol:")
