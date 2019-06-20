module Mkpasswd.UI.Pages.Mkpasswd where

import Prelude
import Data.Array                              ((!!), mapMaybe, null)
import Data.Const                              (Const)
import Data.Either                             (Either(..), note, hush, either)
import Data.Int                                (toNumber, fromString)
import Data.Maybe                              (Maybe(..), fromMaybe, isJust)
import Data.Newtype                            (class Newtype)
import Data.Tuple                              (Tuple, uncurry)
import Data.Traversable                        (traverse)
import Data.Validation.Semigroup               (toEither)
import DOM.HTML.Indexed.StepValue              (StepValue(..))
import Effect                                  (Effect)
import Effect.Aff                              (Aff)
import Formless                              as F
import Halogen                               as H
import Halogen.Component.ChildPath           as HC
import Halogen.Data.Prism                      (type (<\/>), type (\/))
import Halogen.HTML                          as HH
import Halogen.HTML.Events                   as HE
import Halogen.HTML.Properties               as HP

import Mkpasswd                                     (mkpasswd)
import Mkpasswd.Data.Array                          (modifyAt)
import Mkpasswd.Data.FieldType.Mkpasswd             (FieldType(..), labelTxt)
import Mkpasswd.Data.PasswdPolicy                   (PasswdPolicy, defaultLength, defaultPolicy)
import Mkpasswd.Data.PasswdPolicy.Validation        (validate)
import Mkpasswd.Data.Switch                         (Switch)
import Mkpasswd.Data.Switch                       as Switch
import Mkpasswd.Data.Validation                     (ErrorCode(..))
import Mkpasswd.Halogen.Util                        (classes)
import Mkpasswd.UI.Element                        as UI
import Mkpasswd.UI.Routing                          (RouteHash(..), routeHref)
import Mkpasswd.UI.Components.HeaderNav           as Nav
import Mkpasswd.UI.Components.SymbolPolicyFormRow as SymRow

newtype PolRowSlot = PRSlot Int

derive newtype instance eqPolRowSlot  :: Eq PolRowSlot
derive newtype instance ordPolRowSlot :: Ord PolRowSlot

type PolRowQuery = F.Query' PolicyForm Aff

type ChildQuery = Nav.Query <\/> PolRowQuery <\/> SymRow.Query <\/> Const Void
type ChildSlot  = Unit \/ PolRowSlot \/ Unit \/ Void

cpNav :: HC.ChildPath Nav.Query ChildQuery Unit ChildSlot
cpNav = HC.cp1

cpPolRow :: HC.ChildPath PolRowQuery ChildQuery PolRowSlot ChildSlot
cpPolRow = HC.cp2

cpSymRow :: HC.ChildPath SymRow.Query ChildQuery Unit ChildSlot
cpSymRow = HC.cp3

type Input =
    Unit

type Message =
    Maybe String

type State =
    { length :: String
    , policy :: Array (Switch { requiredMinNum :: String, charSet :: Array Int})
    , passwd :: Maybe String
    , errMsg :: Array String
    }

data Query a
    = Generate a
    | Clear a
    | OnInputLength String a
    | OnInputMinNum FieldType (F.Message' PolicyForm) a
    | OnInputSymNum SymRow.Message a

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
          initialState =
              let fp = (\p -> p { requiredMinNum = show p.requiredMinNum }) <$> defaultPolicy
               in const
                     { length : show defaultLength
                     , policy : (Switch.toSwitch Switch.On) <$> fp
                     , passwd : Nothing
                     , errMsg : []
                     }

          render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
          render state =
            HH.main
              [ classes [ if isJust state.passwd then "is-clipped" else "" ] ]
              [ HH.slot' cpNav unit Nav.component unit absurd
              , UI.container
                    [ HH.div
                        [ classes [ "field" ] ]
                        [ HH.label
                            [ HP.for "PasswdLength"
                            , classes [ "pr1", "label" ]
                            ]
                            [ HH.text "Length" ]
                        , HH.span
                            [ classes [ "control" ] ]
                            [ HH.input
                                [ HP.type_ HP.InputNumber
                                , HP.id_ "PasswdLength"
                                , classes [ "input" ]
                                , HP.value state.length
                                , HE.onValueInput $ HE.input OnInputLength
                                , HP.step Any
                                , HP.max (toNumber 100)
                                , HP.min (toNumber 0)
                                ]
                            ]
                        ]
                    , (\i -> HH.slot' cpPolRow (PRSlot 0) F.component i $ HE.input (OnInputMinNum DegitsNum))
                        { initialInputs: F.wrapInputFields
                            { isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 0)
                            , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 0)
                            }
                        , validators
                        , render: renderFormless DegitsNum
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 1) F.component i $ HE.input (OnInputMinNum UppercaseNum))
                        { initialInputs: F.wrapInputFields
                            { isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 1)
                            , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 1)
                            }
                        , validators
                        , render: renderFormless UppercaseNum
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 2) F.component i $ HE.input (OnInputMinNum LowercaseNum))
                        { initialInputs: F.wrapInputFields
                            { isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 2)
                            , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 2)
                            }
                        , validators
                        , render: renderFormless LowercaseNum
                        }
                    , (\i -> HH.slot' cpSymRow unit SymRow.ui i $ HE.input OnInputSymNum)
                        { fieldType: SymbolNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 3)
                        , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 3)
                        , isOpenMulChk: false
                        , allChk: true
                        , chars: (Switch.toSwitch Switch.On) <$> (fromMaybe [] $ (Switch.label >>> _.charSet) <$> (state.policy !! 3))
                        }
                    , HH.div
                        [ classes ["content", "message", "is-danger"] ]
                        [ if null state.errMsg
                            then HH.text ""
                            else HH.div
                                [ classes ["message-body"] ]
                                [ HH.ul
                                    [ classes ["is-marginless"] ] $
                                    (\c -> HH.li [ classes ["is-danger"] ] [ HH.text c ]) <$> state.errMsg
                                ]
                        ]
                    ]
            　, HH.section
                    [ classes [ "level", "sticky-bottom", "box" ] ]
                    [ HH.div
                        [ classes [ "level-item" ] ]
                        [ HH.div
                            [ classes [ "field", "is-grouped" ] ]
                            [ HH.span
                                [ classes [ "control" ] ]
                                [ HH.button
                                    [ classes [ "button", "is-dark" ]
                                    , HE.onClick (HE.input_ Generate)
                                    ]
                                    [ HH.text "Generate" ]
                                ]
                            ]
                        ]
                    ]
              , HH.div
                    [ classes [ "modal", if isJust state.passwd then "is-active" else "" ] ]
                    [ HH.div
                        [ classes [ "modal-background" ]
                        , HE.onClick (HE.input_ Clear)
                        ]
                        []
                    , HH.div
                        [ classes [ "modal-card" ] ]
                        [ HH.header
                            [ classes [ "modal-card-head" ] ]
                            [ HH.h1
                                [ classes [ "modal-card-title" ] ]
                                [ HH.text "Result" ]
                            , HH.button
                                [ classes [ "delete" ]
                                , HP.attr (HH.AttrName "aria-label") "close"
                                , HE.onClick (HE.input_ Clear)
                                ]
                                []
                            ]
                        , HH.section
                            [ classes [ "modal-card-body" ] ]
                            [ HH.div
                                [ classes [ "level" ] ]
                                [ HH.div
                                    [ classes [ "level-item", "has-text-centered", "is-size-3", "text-wrap" ] ]
                                    [ HH.br_
                                    , HH.text $ fromMaybe "" state.passwd
                                    , HH.br_
                                    ]
                                ]
                            ]
                        , HH.footer
                            [ classes [ "modal-card-foot" ] ]
                            [ HH.a
                                [ classes [ "button", "is-dark" ]
                                , HP.href $ routeHref New
                                ]
                                [ HH.text "Save" ]
                            , HH.button
                                [ classes [ "button" ]
                                , HE.onClick (HE.input_ Generate)
                                ]
                                [ HH.text "Regenerate" ]
                            ]
                        ]
                    ]
              ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
          eval (Clear next) = do
              H.modify_ (_ { passwd = Nothing })
              pure next

          eval (Generate next) = do
              s <- H.get
              ep <- H.liftEffect $
                  let len = fromStrLength s.length
                      pol = fromInputPolicyArray s.policy
                      vp  = join $ validateE <$> len <*> pol
                   in case vp of
                           Right prm -> uncurry mkpasswdE prm
                           Left  err -> pure $ Left err
              case ep of
                   Right p -> H.modify_ (_ { errMsg = [], passwd = Just p })
                   Left  e -> H.modify_ (_ { errMsg = e })
              H.raise $ hush ep
              pure next
              where
                    fromStrMinNum :: forall p. { requiredMinNum :: String | p } -> Maybe { requiredMinNum :: Int | p }
                    fromStrMinNum p = p { requiredMinNum = _ } <$> fromString p.requiredMinNum

                    fromStrLength :: String -> Either (Array String) Int
                    fromStrLength l = note ["長さには数値を入れてください"] $ fromString l

                    fromInputPolicyArray :: Array (Switch { requiredMinNum :: String, charSet :: Array Int }) -> Either (Array String) (Array PasswdPolicy)
                    fromInputPolicyArray p = note ["各文字種ごとの必要最低数には数値を入れてください"] $ traverse fromStrMinNum (mapMaybe Switch.toMaybe p)

                    validateE :: Int -> Array PasswdPolicy -> Either (Array String) (Tuple Int (Array PasswdPolicy))
                    validateE l p = either (\e -> Left $ showErr <$> e) (\v -> Right v) $ toEither $ validate l p

                    mkpasswdE :: Int -> Array PasswdPolicy -> Effect (Either (Array String) String)
                    mkpasswdE l a = note [showErr Unknown] <$> mkpasswd l a

          eval (OnInputLength mi next) = do
              H.modify_ (_ { length = mi })
              pure next

          eval (OnInputMinNum fieldType m next) = case m of
             F.Submitted formOutput -> do
                 let r = F.unwrapOutputFields formOutput
                 modifyPolicyRow fieldType (r {requiredMinNum = show r.requiredMinNum}) next
             _ -> pure next

          eval (OnInputSymNum r next) =
             let idx = fieldIdx r.fieldType
                 pm  = { isUsed: r.isUsed
                       , requiredMinNum: r.requiredMinNum
                       }
              in do
                 s <- H.get
                 H.modify_ (_ { policy = modifyAt idx (map (_ { charSet = mapMaybe Switch.toMaybe r.chars })) s.policy })
                 modifyPolicyRow r.fieldType pm next

          fieldIdx :: FieldType -> Int
          fieldIdx DegitsNum    = 0
          fieldIdx UppercaseNum = 1
          fieldIdx LowercaseNum = 2
          fieldIdx SymbolNum    = 3

          modifyPolicyRow :: forall a. FieldType -> {isUsed :: Boolean, requiredMinNum :: String} -> a -> H.ParentDSL State Query ChildQuery ChildSlot Message Aff a
          modifyPolicyRow fieldType r next = do
             let idx = fieldIdx fieldType
             s <- H.get
             let np = modifyAt idx (\w -> (_ { requiredMinNum = r.requiredMinNum }) <$> turn r.isUsed w) s.policy
             H.modify_ (_ { policy = np })
             pure next

          turn :: forall a. Boolean -> Switch a -> Switch a
          turn flg = if flg then Switch.on else Switch.off


showErr :: ErrorCode -> String
showErr =
    case _ of
        OutOfRange   -> "長過ぎます"
        ValueMissing -> "入力してください"
        EmptyCharSet -> "指定された文字種が空です"
        TooShort     -> "長さは文字種ごとの必要最低数の総和よりも大きくしてください"
        TypeMismatch -> "各文字種ごとの必要最低数には数値を入れてください"
        Unknown      -> "なんかエラーになったんでリロードしてください"


-- Formless

newtype PolicyForm r f = PolicyForm (r
  ( requiredMinNum :: f (Array ErrorCode) String Int
  , isUsed         :: f (Array ErrorCode) Boolean Boolean
  ))
derive instance newtypeForm :: Newtype (PolicyForm r f) _

proxy :: F.SProxies PolicyForm
proxy = F.mkSProxies (F.FormProxy :: F.FormProxy PolicyForm)

validators :: PolicyForm Record (F.Validation PolicyForm Aff)
validators = PolicyForm
    { requiredMinNum : isInt
    , isUsed         : F.hoistFn_ (\i -> i)
    }
    where
        isInt = F.hoistFnE_ $ note [TypeMismatch] <<< fromString

renderFormless :: forall m. FieldType -> F.State PolicyForm m -> F.HTML' PolicyForm m
renderFormless fieldType fstate =
    let isUsed = F.getInput proxy.isUsed fstate.form
     in HH.div
        [ classes [ "field" ] ]
        [ HH.label
            [ classes [ "label" ] ]
            [ HH.text (labelTxt fieldType) ]
        , HH.div
            [ classes [ "field", "has-addons" ] ]
            [ HH.span
                [ classes [ "control" ] ]
                [ HH.label
                    [ classes [ "button", "checkbox" ]
                    ]
                    [ HH.input
                        [ HP.type_ HP.InputCheckbox
                        , HP.checked isUsed
                        , classes [ "mr1" ]
                        , HE.onChecked $ HE.input $ F.setValidate proxy.isUsed
                        ]
                    , HH.text "use it"
                    ]
                ]
            , HH.span
                [ classes [ "control", "is-expanded" ] ]
                [ HH.input
                    [ HP.type_ HP.InputNumber
                    , HP.id_ (show fieldType)
                    , classes
                        [ "input"
                        , if (isJust $ F.getError proxy.requiredMinNum fstate.form)
                            then "is-danger"
                            else ""
                        ]
                    , HP.value $ F.getInput proxy.requiredMinNum fstate.form
                    , HE.onValueInput $ HE.input $ F.setValidate proxy.requiredMinNum
                    , HP.step Any
                    , HP.min (toNumber 0)
                    , HP.max (toNumber 100)
                    , HP.disabled (not isUsed)
                    ]
                ]
            ]
        , HH.ul
            [ classes ["help", "is-danger"] ]
            $ (\msg -> HH.li_ [ HH.text (showErr msg) ])
            <$> (fromMaybe [] $ F.getError proxy.requiredMinNum fstate.form)
        ]

