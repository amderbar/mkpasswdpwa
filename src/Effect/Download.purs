module Effect.Download (csvObjectUrl) where

import Prelude
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Web.File.Blob as Blob
import Web.File.Url as Url

-- | CSV 文字列から、ダウンロード用の Blob Object URL を生成する。
-- | 呼び出し側は、不要になったら Url.revokeObjectURL で解放すること。
csvObjectUrl :: String -> Effect String
csvObjectUrl content =
  Url.createObjectURL $ Blob.fromString content (MediaType "text/csv;charset=utf-8;")
