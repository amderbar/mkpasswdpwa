module Mkpasswd.Data.FieldType.Mkpasswd where

import Prelude                    (class Show)
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)

data FieldType
    = DegitsNum
    | UppercaseNum
    | LowercaseNum
    | SymbolNum

derive instance genericFieldType :: Generic FieldType _
instance showFieldType :: Show FieldType where
    show = genericShow

labelTxt :: FieldType -> String
labelTxt DegitsNum    = "すうじ"
labelTxt UppercaseNum = "英大字"
labelTxt LowercaseNum = "英小字"
labelTxt SymbolNum    = "きごう"
