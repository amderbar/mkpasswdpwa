module Main.CLI (main) where

import Prelude
import ArgParse.Basic (ArgParser, argument, default, flagHelp, fromRecord, int, parseArgs, printArgError, unformat)
import Data.Array (drop)
import Data.Count (Count, fromCount, toCount)
import Data.Either (Either(..), note)
import Data.Length (Length, fromLength, toLength)
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
        , parser: policyArg <* flagHelp
        }
  args <- Process.argv <#> drop 2
  case execParser args of
    Right policy -> logShow =<< randomSampleOne (genPasswd policy)
    Left err -> log (printArgError err)
  where
  argParser { header, desc, parser } = parseArgs header desc parser

policyArg :: ArgParser Policy
policyArg =
  fromRecord
    { length:
        argument [ "--length", "-l" ] "Required length."
          # int
          # default 9
          # lengthArg
    , digitNum:
        argument [ "--digit", "-d" ] "Minimum number of digits to include."
          # int
          # default 2
          # countArg
    , lowercaseNum:
        argument [ "--lowercase", "-c" ] "Minimum number of lowercase characters to include."
          # int
          # default 2
          # countArg
    , capitalNum:
        argument [ "--capital", "-C" ] "Minimum number of capital letters to include."
          # int
          # default 2
          # countArg
    , symbolNum:
        argument [ "--symbol", "-s" ] "Minimum number of symbols to include."
          # int
          # default 1
          # countArg
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
