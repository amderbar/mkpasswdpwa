module Mkpasswd.UI.Pages.Store where

import Prelude
import Mkpasswd.Data.States       (FormData, initialForm, validate)
import Mkpasswd.Data.Validation   (ErrorCode(..))
import Mkpasswd.Halogen.Util      (classes)
import Mkpasswd.UI.Routing        (RouteHash(..), routeHref)
import Data.Either                (Either(..))
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Show      (genericShow)
import Data.Maybe                 (Maybe(..), fromMaybe)
import Data.Validation.Semigroup  (toEither)
import Effect.Aff                 (Aff)
import Halogen                 as H
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP
import Routing.Hash               (setHash)

type Input = Maybe FormData

data Message = SavePasswd FormData

type State =
    { form  :: FormData
    , error :: Maybe (Array String)
    }

data FeildType
    = AccountInput
    | Passwdinput
    | NoteTextarea

derive instance genericFeildType :: Generic FeildType _
instance showFeildType :: Show FeildType where
    show = genericShow

data Query a
    = UpdateAccount String a
    | UpdatePasswd String a
    | UpdateNote String a
    | Save a

ui :: H.Component HH.HTML Query Input Message Aff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: Input -> State
          initialState inp =
              let f = fromMaybe initialForm inp
              in { form : f
                 , error: Nothing
                 }

          render :: State -> H.ComponentHTML Query
          render state =
                HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1 [ classes [ "center" ] ] [ HH.text "Store" ]
                    , errorView $ show <$> state.error
                    , txtInput AccountInput state.form.account
                    , txtInput Passwdinput state.form.passwd
                    , txtArea NoteTextarea state.form.note
                    , HH.div
                        [ classes [ "flex", "justify-center" ] ]
                        [ HH.button
                            [ classes [ "btn", "btn-primary", "mr2" ]
                            , HE.onClick (HE.input_ Save)
                            ]
                            [ HH.text "Save" ]
                        , HH.a
                            [ classes [ "btn", "btn-primary", "bg-gray" ]
                            , HP.href $ routeHref List
                            ]
                            [ HH.text "Cancel" ]
                        ]
                    ]
          labelTxt AccountInput  = "アカウントID："
          labelTxt Passwdinput   = "パスワード："
          labelTxt NoteTextarea  = "備考："
          queryType AccountInput = UpdateAccount
          queryType Passwdinput  = UpdatePasswd
          queryType NoteTextarea = UpdateNote
          txtInput feildType currVal = txtForm feildType $
                HH.input
                   [ HP.type_ HP.InputText
                   , HP.id_ $ show feildType
                   , classes [ "col", "col-3", "input" ]
                   , HP.value currVal
                   , HE.onValueInput $ HE.input $ queryType feildType
                   ]
          txtArea feildType currVal = txtForm feildType $
                HH.textarea
                   [ HP.id_ $ show feildType
                   , classes [ "col", "col-3", "input" ]
                   , HP.value currVal
                   , HE.onValueInput $ HE.input $ queryType feildType
                   ]
          txtForm feildType inpHtmlElm =
                HH.div
                   [ classes [ "flex-none", "clearfix" ] ]
                   [ HH.label
                        [ HP.for $ show feildType
                        , classes [ "pr1", "col", "col-4", "right-align", "align-baseline", "label" ]
                        ]
                        [ HH.text $ labelTxt feildType ]
                   , inpHtmlElm
                   ]
          errorView  Nothing = HH.text ""
          errorView  (Just error) =
              HH.p [ classes [ "h3" , "center" , "border" , "border-red" ] ]
                   [ HH.text error ]

          errorMsg OutOfRange   = "長過ぎます"
          errorMsg ValueMissing = "入力してください"
          errorMsg EmptyCharSet = "指定された文字種が空です"
          errorMsg TooShort     = "長さは文字種ごとの必要最低数の総和よりも大きくしてください"
          errorMsg Unknown      = "なんかエラーになったんでリロードしてください"

          eval :: Query ~> H.ComponentDSL State Query Message Aff
          eval (UpdateAccount newVal next) = do
             s <- H.get
             H.modify_ (_ { form = s.form { account = newVal } })
             pure next
          eval (UpdatePasswd newVal next) = do
             s <- H.get
             H.modify_ (_ { form = s.form { passwd = newVal } })
             pure next
          eval (UpdateNote newVal next) = do
             s <- H.get
             H.modify_ (_ { form = s.form { note = newVal } })
             pure next
          eval (Save next) = do
             s <- H.get
             case (toEither $ validate s.form) of
                  Right f -> do
                     H.liftEffect $ setHash $ routeHref List
                     H.raise $ SavePasswd f
                  Left  e -> H.modify_ (_ { error = Just (errorMsg <$> e) })
             pure next
