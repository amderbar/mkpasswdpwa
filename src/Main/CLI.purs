module Main.CLI (main) where

import Prelude

import ArgParse.Basic (ArgParser, argument, choose, default, flagHelp, fromRecord, int, parseArgs, printArgError, unfolded1, unformat)
import Data.Array (drop)
import Data.Char.GenSource (digits, lowercases, mkGenSource, uppercases)
import Data.Char.Subset (symbols)
import Data.Count (Count, fromCount, toCount)
import Data.Either (Either(..), note)
import Data.Length (Length, fromLength, toLength)
import Data.Passwd.Gen (genPasswd)
import Data.Policy (Policy, defaultPolicy)
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

    symbolNum =
      argument [ "--symbol", "-s" ] "Minimum number of symbols to include."
        # int
        # countArg

    charTypeConf =
      choose "required charactor types"
        [ digitNumArg <#> { count: _, genSrc: digits }
        , lowercaseNum <#> { count: _, genSrc: lowercases }
        , capitalNum <#> { count: _, genSrc: uppercases }
        , symbolNum <#> { count: _, genSrc: mkGenSource symbols }
        ]
  in
    fromRecord
      { length:
          argument [ "--length", "-l" ] "Required length."
            # int
            # lengthArg
            # default defaultPolicy.length
      , required: unfolded1 charTypeConf # default defaultPolicy.required
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
