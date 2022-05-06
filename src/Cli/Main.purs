module Mkpasswd.Cli.Main
  ( main
  ) where

import Prelude
import ArgParse.Basic (ArgParser, ArgError, argument, default, flagHelp, fromRecord, int, parseArgs, printArgError, unformat)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (drop)
import Data.Array.NonEmpty (fromArray)
import Data.Char.Gen (genDigitChar, genAlphaLowercase, genAlphaUppercase)
import Data.Char.Gen.Symbols (genSymbolChar)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Data.PasswdPolicy (PasswdPolicy)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Mkpasswd (mkpasswd)
import Node.Process (argv) as Process

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
  ret <- runExceptT $ go (execParser args)
  case ret of
    Right r -> log r
    Left e -> log e
  where
  argParser { header, desc, parser } = parseArgs header desc parser

go :: Either ArgError Args -> ExceptT String Effect String
go parseRet = execGen =<< withExceptT printArgError (except parseRet)
  where
  execGen :: Args -> ExceptT String Effect String
  execGen args = do
    policy <- except $ note "Failed to construct PasswdPolicy" (toPasswdPolicy args)
    ExceptT $ Right <$> mkpasswd policy

  toPasswdPolicy :: forall m. MonadRec m => MonadGen m => Args -> Maybe (PasswdPolicy m)
  toPasswdPolicy args =
    let
      seed =
        fromArray
          [ Tuple args.digitNum genDigitChar
          , Tuple args.capitalNum genAlphaUppercase
          , Tuple args.lowercaseNum genAlphaLowercase
          , Tuple args.symbolNum genSymbolChar
          ]
    in
      { length: args.length, required: _ } <$> seed

type Args
  = { length :: Int
    , digitNum :: Int
    , lowercaseNum :: Int
    , capitalNum :: Int
    , symbolNum :: Int
    }

policyArg :: ArgParser Args
policyArg =
  fromRecord
    { length:
        argument [ "--length", "-l" ] "Required length."
          # int
          # range 1 100
          # default 9
    , digitNum:
        argument [ "--digit", "-d" ] "Minimum number of digits to include."
          # int
          # range 0 100
          # default 2
    , lowercaseNum:
        argument [ "--lowercase", "-c" ] "Minimum number of lowercase characters to include."
          # int
          # range 0 100
          # default 2
    , capitalNum:
        argument [ "--capital", "-C" ] "Minimum number of capital letters to include."
          # int
          # range 0 100
          # default 2
    , symbolNum:
        argument [ "--symbol", "-s" ] "Minimum number of symbols to include."
          # int
          # range 0 100
          # default 1
    }

range :: Int -> Int -> ArgParser Int -> ArgParser Int
range l u = unformat "INT" chk
  where
  chk i =
    if between l u i then
      Right i
    else
      Left $ "Expected INT between " <> (show l) <> " to " <> (show u)
