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
labelTxt DegitsNum    = "Numeral"
labelTxt UppercaseNum = "Uppercase Alphabet"
labelTxt LowercaseNum = "Lowercase Alphabet"
labelTxt SymbolNum    = "Symbol"
