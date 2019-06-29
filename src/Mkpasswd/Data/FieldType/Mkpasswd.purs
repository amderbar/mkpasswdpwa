module Mkpasswd.Data.FieldType.Mkpasswd where

import Prelude                    (class Show, class Eq, class Ord)
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

derive instance eqFieldType  :: Eq FieldType
derive instance ordFieldType :: Ord FieldType

labelTxt :: FieldType -> String
labelTxt DegitsNum    = "Numeral"
labelTxt UppercaseNum = "Uppercase Alphabet"
labelTxt LowercaseNum = "Lowercase Alphabet"
labelTxt SymbolNum    = "Symbol"
