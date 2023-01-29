module Main.CLI (main) where

import Prelude
import ArgParse.Basic (ArgParser, argument, choose, default, flag, flagHelp, fromRecord, int, parseArgs, printArgError, unformat)
import Data.Array (drop)
import Data.Char.Symbols (symbols, toNonEmptySymbolCharArrray)
import Data.Count (Count, fromCount, toCount)
import Data.Either (Either(..), note)
import Data.Length (Length, fromLength, toLength)
import Data.Maybe (Maybe(..))
import Data.Passwd.Gen (genPasswd)
import Data.Policy (Policy)
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
        , parser: unformat "" genPasswd policyArg <* flagHelp
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
        # default 2
        # countArg

    lowercaseNum =
      argument [ "--lowercase", "-c" ] "Minimum number of lowercase characters to include."
        # int
        # default 2
        # countArg

    capitalNum =
      argument [ "--capital", "-C" ] "Minimum number of capital letters to include."
        # int
        # default 2
        # countArg

    symbolNum =
      argument [ "--symbol", "-s" ] "Minimum number of symbols to include."
        # int
        # default 1
        # countArg

    symbolSet =
      argument [ "--symbol-set" ] "Avairable symbols to include."
        # unformat "STR" toNonEmptySymbolCharArrray
  in
    fromRecord
      { length:
          argument [ "--length", "-l" ] "Required length."
            # int
            # default 9
            # lengthArg
      , digitNum:
          choose "digits"
            [ Just <$> digitNumArg
            , flag [ "-nd", "--no-digits" ] "exclude digits." $> Nothing
            ]
            # default (toCount 2)
      , lowercaseNum:
          choose "lowercase letters"
            [ Just <$> lowercaseNum
            , flag [ "-nc", "--no-lowercases" ] "exclude lowercase letters." $> Nothing
            ]
            # default (toCount 2)
      , capitalNum:
          choose "capital letters"
            [ Just <$> capitalNum
            , flag [ "-nC", "--no-capitals" ] "exclude capital letters." $> Nothing
            ]
            # default (toCount 2)
      , symbolNum:
          choose "symbols"
            [ Just <$> fromRecord { count: symbolNum, charset: symbolSet }
            , flag [ "-ns", "--no-symbols" ] "exclude symbols." $> Nothing
            ]
            # default ({ count: _, charset: symbols } <$> toCount 1)
      }

lengthArg :: ArgParser Int -> ArgParser Length
lengthArg = unformat "INT" (note msg <<< toLength)
  where
  msg =
    let
      b = fromLength bottom

      t = fromLength top
    in
      "Expected INT between " <> (show b) <> " to " <> (show t)

countArg :: ArgParser Int -> ArgParser Count
countArg = unformat "INT" (note msg <<< toCount)
  where
  msg =
    let
      b = fromCount bottom

      t = fromCount top
    in
      "Expected INT between " <> (show b) <> " to " <> (show t)