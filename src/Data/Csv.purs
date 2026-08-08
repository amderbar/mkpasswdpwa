module Data.Csv
  ( encodeCsv
  , encodeField
  , formDataToRows
  ) where

import Prelude
import Data.Array (intercalate)
import Data.String as String
import Data.States (FormData)

-- | RFC 4180 準拠のフィールドエスケープ。
-- | カンマ・ダブルクォート・改行を含む場合はダブルクォートで囲み、
-- | 内部のダブルクォートは "" にエスケープする。
encodeField :: String -> String
encodeField field =
  if needsQuoting then
    "\"" <> String.replaceAll (String.Pattern "\"") (String.Replacement "\"\"") field <> "\""
  else
    field
  where
  needsQuoting =
    String.contains (String.Pattern ",") field
      || String.contains (String.Pattern "\"") field
      || String.contains (String.Pattern "\n") field
      || String.contains (String.Pattern "\r") field

-- | 2次元配列（行×列）を CSV 文字列に変換する。改行は CRLF（Excel 互換）。
encodeCsv :: Array (Array String) -> String
encodeCsv rows = intercalate "\r\n" (map (intercalate "," <<< map encodeField) rows)

-- | FormData の配列をヘッダ行付きの Array (Array String) に変換する。
formDataToRows :: Array FormData -> Array (Array String)
formDataToRows list =
  [ [ "account", "passwd", "note" ] ] <> map toRow list
  where
  toRow fd = [ fd.account, fd.passwd, fd.note ]
