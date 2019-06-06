module Mkpasswd.UI.Pages.Store where

import Prelude
import Mkpasswd.Data.States       (FormData, initialForm, validate)
import Mkpasswd.Data.Validation   (ErrorCode(..))
import Mkpasswd.Halogen.Util      (classes)
import Mkpasswd.UI.Components.HeaderNav as Nav
import Mkpasswd.UI.Element     as UI
import Mkpasswd.UI.Routing        (RouteHash(..), routeHref)
import Data.Array                 (null)
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

type ChildQuery = Nav.Query
type ChildSlot = Unit

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
  H.parentComponent
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

          render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
          render state =
            HH.main_
              [ HH.slot unit Nav.component unit absurd
              , UI.container
                    [ errorView state.error
                    , txtInput AccountInput state.form.account
                    , txtInput Passwdinput state.form.passwd
                    , txtArea NoteTextarea state.form.note
                    , HH.div
                        [ classes [ "field", "is-grouped" ] ]
                        [ HH.span
                            [ classes [ "control" ] ]
                            [ HH.button
                                [ classes [ "button", "is-dark" ]
                                , HE.onClick (HE.input_ Save)
                                ]
                                [ HH.text "Save" ]
                            ]
                        , HH.span
                            [ classes [ "control" ] ]
                            [ HH.a
                                [ classes [ "button" ]
                                , HP.href $ routeHref List
                                ]
                                [ HH.text "Cancel" ]
                            ]
                        ]
                    ]
              ]
          labelTxt AccountInput  = "Title"
          labelTxt Passwdinput   = "The Work"
          labelTxt NoteTextarea  = "Description"
          queryType AccountInput = UpdateAccount
          queryType Passwdinput  = UpdatePasswd
          queryType NoteTextarea = UpdateNote
          txtInput feildType currVal = txtForm feildType $
                HH.input
                   [ HP.type_ HP.InputText
                   , HP.id_ $ show feildType
                   , classes [ "input" ]
                   , HP.value currVal
                   , HE.onValueInput $ HE.input $ queryType feildType
                   ]
          txtArea feildType currVal = txtForm feildType $
                HH.textarea
                   [ HP.id_ $ show feildType
                   , classes [ "textarea" ]
                   , HP.value currVal
                   , HE.onValueInput $ HE.input $ queryType feildType
                   ]
          txtForm feildType inpHtmlElm =
                HH.div
                   [ classes [ "field" ] ]
                   [ HH.label
                        [ HP.for $ show feildType
                        , classes [ "label" ]
                        ]
                        [ HH.text $ labelTxt feildType ]
                   , HH.div
                        [ classes ["control"] ]
                        [ inpHtmlElm ]
                   ]
          errorView  Nothing = HH.text ""
          errorView  (Just error) =
            HH.div
                [ classes ["content", "message", "is-danger"] ]
                [ if null error
                    then HH.text ""
                    else HH.div
                        [ classes ["message-body"] ]
                        [ HH.ul
                            [ classes ["is-marginless"] ] $
                            (\c -> HH.li [ classes ["is-danger"] ] [ HH.text c ]) <$> error
                        ]
                ]

          errorMsg OutOfRange   = "長過ぎます"
          errorMsg ValueMissing = "入力してください"
          errorMsg EmptyCharSet = "指定された文字種が空です"
          errorMsg TooShort     = "長さは文字種ごとの必要最低数の総和よりも大きくしてください"
          errorMsg Unknown      = "なんかエラーになったんでリロードしてください"

          eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
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
