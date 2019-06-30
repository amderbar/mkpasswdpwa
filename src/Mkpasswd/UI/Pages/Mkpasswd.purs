module Mkpasswd.UI.Pages.Mkpasswd where

import Prelude
import Control.Alt                             ((<|>))
import Data.Array                              ((!!), filter, mapMaybe)
import Data.Const                              (Const)
import Data.Either                             (note)
import Data.Int                                (toNumber)
import Data.Maybe                              (Maybe(..), fromMaybe, isJust)
import Data.Newtype                            (class Newtype)
import Data.Traversable                        (for, traverse)
import DOM.HTML.Indexed.StepValue              (StepValue(..))
import Effect.Aff                              (Aff)
import Formless                              as F
import Halogen                               as H
import Halogen.Component.ChildPath           as HC
import Halogen.Data.Prism                      (type (<\/>), type (\/))
import Halogen.HTML                          as HH
import Halogen.HTML.Events                   as HE
import Halogen.HTML.Properties               as HP
import Mkpasswd                                     (mkpasswd)
import Mkpasswd.Data.Ascii                          (CharCode)
import Mkpasswd.Data.FieldType.Mkpasswd             (FieldType(..), labelTxt)
import Mkpasswd.Data.PasswdPolicy                   (PasswdPolicy, passwdPolicy, defaultLength, defaultPolicy, requiredMinLength)
import Mkpasswd.Data.Switch                         (Switch)
import Mkpasswd.Data.Switch                       as Switch
import Mkpasswd.Data.Validation                     (ErrorCode(..))
import Mkpasswd.Data.Validation                   as V
import Mkpasswd.Halogen.Util                        (classes)
import Mkpasswd.UI.Components.HeaderNav           as Nav
import Mkpasswd.UI.Components.MultiChkboxes       as MulChk
import Mkpasswd.UI.Element                        as UI
import Mkpasswd.UI.Routing                          (RouteHash(..), routeHref)

maxLength :: Int
maxLength = 100

minLength :: Int
minLength = 0

type ChildQuery = Nav.Query <\/> FormQuery <\/> Const Void
type ChildSlot  = Unit \/ Unit \/ Void

cpNav :: HC.ChildPath Nav.Query ChildQuery Unit ChildSlot
cpNav = HC.cp1

cpForm :: HC.ChildPath FormQuery ChildQuery Unit ChildSlot
cpForm = HC.cp2

type Input =
    Unit

type Message =
    Maybe String

type State =
    { passwd :: Maybe String
    , errMsg :: Array String
    }

data Query a
    = Generate a
    | Clear a
    | OnFormMsg (F.Message Query Form) a
    | OnPolFromMsg (F.Message Query PolicyForm) a

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
          initialState = const
                     { passwd : Nothing
                     , errMsg : []
                     }

          render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
          render state =
            HH.main
              [ classes [ if isJust state.passwd then "is-clipped" else "" ] ]
              [ HH.slot' cpNav unit Nav.component unit absurd
              , (\i -> HH.slot' cpForm unit F.component i $ HE.input OnFormMsg)
                    { initialInputs: F.wrapInputFields { length: show defaultLength, policy : Nothing }
                    , validators
                    , render: renderFormless
                    }
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
          eval (OnFormMsg m next) = case m of
              F.Emit q -> eval q *> pure next
              _ -> pure next

          eval (OnPolFromMsg m next) = case m of
              F.Emit q -> eval q *> pure next
              _ -> pure next

          eval (Clear next) = do
              H.modify_ (_ { passwd = Nothing })
              pure next

          eval (Generate next) = next <$ do
              let fieldTypes =
                    [ DegitsNum
                    , UppercaseNum
                    , LowercaseNum
                    , SymbolNum
                    ]
              policyForm <- for fieldTypes \ft -> H.query' cpForm unit $ F.send ft F.submitReply
              let policy = (map <<< map) F.unwrapOutputFields $ traverse join policyForm

              _ <- H.query' cpForm unit $ F.set_ proxy.policy policy
              res <- H.query' cpForm unit F.submitReply

              maybeP <- H.liftEffect $ join <$> traverse mkpasswdFormless (join res)
              when (isJust maybeP) $ H.modify_ (_ { passwd = maybeP })

              where
                  mkpasswdFormless r =
                      let s = F.unwrapOutputFields r
                       in mkpasswd s.length s.policy

-- Formless

type FormQuery = F.Query Query PolRowQuery PolRowSlot Form Aff

newtype Form r f = Form (r
  ( length :: f (Array ErrorCode) String Int
  , policy :: f (Array ErrorCode) (Maybe (Array PolicyInfo)) (Array PasswdPolicy)
  ))
derive instance newtypeForm :: Newtype (Form r f) _

proxy :: F.SProxies Form
proxy = F.mkSProxies (F.FormProxy :: F.FormProxy Form)

validators :: Form Record (F.Validation Form Aff)
validators = Form
    { length : required >>> int >>> range >>> enoughLong
    , policy : exists >>> policy >>> nonEmpty
    }
    where
      required = F.hoistFnE_ (V.chk ValueMissing V.requiredRule)
      int = F.hoistFnE_ (V.int TypeMismatch)
      range = F.hoistFnE_ (V.chk OutOfRange $ V.rangeRule minLength maxLength)
      exists = F.hoistFnE_ $ note [Invalid]
      policy = F.hoistFn_ \i -> (\p -> passwdPolicy p.requiredMinNum p.charSet) <$> filter _.isUsed i
      nonEmpty = F.hoistFnE_ (V.chk EmptyCharSet V.nonEmptyRule)
      enoughLong = F.hoistFnE \form ->
        let p = fromMaybe [] $ F.getOutput proxy.policy form
        in V.chk TooShort $ V.minRule (requiredMinLength p)

renderFormless :: F.State Form Aff -> F.HTML Query PolRowQuery PolRowSlot Form Aff
renderFormless fstate =
    UI.container
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
                    , classes
                        [ "input"
                        , if (isJust $ F.getError proxy.length fstate.form)
                            then "is-danger"
                            else ""
                        ]
                    , HP.value $ F.getInput proxy.length fstate.form
                    , HE.onValueInput $ HE.input $ F.setValidate proxy.length
                    , HP.step Any
                    , HP.max (toNumber maxLength)
                    , HP.min (toNumber minLength)
                    ]
                ]
            , HH.ul
                [ classes ["help", "is-danger"] ]
                $ (\msg -> HH.li_ [ HH.text (showErr msg) ])
                <$> (fromMaybe [] $ F.getError proxy.length fstate.form)
            ]
        , (\i -> HH.slot DegitsNum F.component i $
            HE.input (F.raise <<< H.action <<< OnPolFromMsg)
          )
            { initialInputs: F.wrapInputFields
                { isUsed: true
                , requiredMinNum: fromMaybe "" $ show <<< _.requiredMinNum <$> (defaultPolicy !! 0)
                , charSet: (Switch.toSwitch Switch.On) <$> (fromMaybe [] $ _.charSet <$> (defaultPolicy !! 0))
                }
            , validators: validatorsPolicyRow
            , render: renderPolicyRow DegitsNum
            }
        , (\i -> HH.slot UppercaseNum F.component i $
            HE.input (F.raise <<< H.action <<< OnPolFromMsg)
          )
            { initialInputs: F.wrapInputFields
                { isUsed: true
                , requiredMinNum: fromMaybe "" $ show <<< _.requiredMinNum <$> (defaultPolicy !! 1)
                , charSet: (Switch.toSwitch Switch.On) <$> (fromMaybe [] $ _.charSet <$> (defaultPolicy !! 1))
                }
            , validators: validatorsPolicyRow
            , render: renderPolicyRow UppercaseNum
            }
        , (\i -> HH.slot LowercaseNum F.component i $
            HE.input (F.raise <<< H.action <<< OnPolFromMsg)
          )
            { initialInputs: F.wrapInputFields
                { isUsed: true
                , requiredMinNum: fromMaybe "" $ show <<< _.requiredMinNum <$> (defaultPolicy !! 2)
                , charSet: (Switch.toSwitch Switch.On) <$> (fromMaybe [] $ _.charSet <$> (defaultPolicy !! 2))
                }
            , validators: validatorsPolicyRow
            , render: renderPolicyRow LowercaseNum
            }
        , (\i -> HH.slot SymbolNum F.component i $
            HE.input (F.raise <<< H.action <<< OnPolFromMsg)
          )
            { initialInputs: F.wrapInputFields
                { isUsed: true
                , requiredMinNum: fromMaybe "" $ show <<< _.requiredMinNum <$> (defaultPolicy !! 3)
                , charSet: (Switch.toSwitch Switch.On) <$> (fromMaybe [] $ _.charSet <$> (defaultPolicy !! 3))
                }
            , validators: validatorsPolicyRow
            , render: renderPolicyRow SymbolNum
            }
        , HH.ul
            [ classes ["help", "is-danger"] ]
            $ (\msg -> HH.li_ [ HH.text (showErr msg) ])
            <$> (fromMaybe [] $ F.getError proxy.policy fstate.form)
        ]

showErr :: ErrorCode -> String
showErr =
    case _ of
        OutOfRange   -> (show minLength) <> "と" <> (show maxLength)　<> "の間で入力してください"
        ValueMissing -> "整数を入力してください"
        EmptyCharSet -> "使用する文字を指定してください"
        TooShort     -> "長さは文字種ごとの必要最低数の総和よりも大きくしてください"
        TypeMismatch -> "整数を入力してください"
        Invalid      -> "エラーを解消してください"
        Unknown      -> "なんかエラーになったんでリロードしてください"

-- PolicyForm

type PolRowQuery = F.Query Query MulChk.Query Unit PolicyForm Aff
type PolRowSlot = FieldType

type PolicyInfo = Record (PolicyRow F.OutputType)

newtype PolicyForm r f = PolicyForm (r (PolicyRow f))
derive instance newtypePolicyForm :: Newtype (PolicyForm r f) _

type PolicyRow f =
  ( requiredMinNum :: f (Array ErrorCode) String Int
  , isUsed         :: f (Array ErrorCode) Boolean Boolean
  , charSet        :: f (Array ErrorCode) (Array (Switch CharCode)) (Array CharCode)
  )

proxyPolicyRow :: F.SProxies PolicyForm
proxyPolicyRow = F.mkSProxies (F.FormProxy :: F.FormProxy PolicyForm)

validatorsPolicyRow :: PolicyForm Record (F.Validation PolicyForm Aff)
validatorsPolicyRow = PolicyForm
    { requiredMinNum : (unUsed 0) <|> required >>> int >>> range
    , isUsed         : F.hoistFn_ identity
    , charSet        : (unUsed [0]) <|> unSwitchAll >>> nonEmpty
    }
    where
      unUsed :: forall i o. o -> F.Validation PolicyForm Aff (Array ErrorCode) i o
      unUsed o = F.hoistFnE $ \form _ -> V.chk Unknown (\_ -> not $ F.getInput proxyPolicyRow.isUsed form) o
      required = F.hoistFnE_ (V.chk ValueMissing V.requiredRule)
      int = F.hoistFnE_ (V.int TypeMismatch)
      range = F.hoistFnE_ (V.chk OutOfRange $ V.rangeRule minLength maxLength)
      unSwitchAll = F.hoistFn_ (mapMaybe Switch.toMaybe)
      nonEmpty = F.hoistFnE_ (V.chk EmptyCharSet V.nonEmptyRule)

renderPolicyRow :: FieldType -> F.State PolicyForm Aff -> F.HTML Query MulChk.Query Unit PolicyForm Aff
renderPolicyRow fieldType fstate =
    let isUsed = F.getInput proxyPolicyRow.isUsed fstate.form
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
                        , HE.onChecked $ HE.input $ F.setValidate proxyPolicyRow.isUsed
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
                        , if (isJust $ F.getError proxyPolicyRow.requiredMinNum fstate.form)
                            then "is-danger"
                            else ""
                        ]
                    , HP.value $ F.getInput proxyPolicyRow.requiredMinNum fstate.form
                    , HE.onValueInput $ HE.input $ F.setValidate proxyPolicyRow.requiredMinNum
                    , HP.step Any
                    , HP.min (toNumber minLength)
                    , HP.max (toNumber maxLength)
                    , HP.disabled (not isUsed)
                    ]
                ]
            ]
        , HH.ul
            [ classes ["help", "is-danger"] ]
            $ (\msg -> HH.li_ [ HH.text (showErr msg) ])
            <$> ( (fromMaybe [] $ F.getError proxyPolicyRow.requiredMinNum fstate.form)
                <> ((fromMaybe [] $ F.getError proxyPolicyRow.charSet fstate.form))
                )
        , if fieldType == SymbolNum
            then (\i -> HH.slot unit MulChk.ui i (HE.input $ F.setValidate proxyPolicyRow.charSet))
                    { allChk: true
                    , chars : F.getInput proxyPolicyRow.charSet fstate.form
                    , isOpenMulChk: false
                    , disabled: not isUsed
                    }
            else HH.text ""
        ]
