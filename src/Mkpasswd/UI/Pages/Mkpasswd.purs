module Mkpasswd.UI.Pages.Mkpasswd where

import Prelude
import Data.Array                              ((!!), mapMaybe, null)
import Data.Const                              (Const)
import Data.Either                             (Either(..), note, hush, either)
import Data.Int                                (toNumber, fromString)
import Data.Maybe                              (Maybe(..), fromMaybe, isJust)
import Data.Tuple                              (Tuple, uncurry)
import Data.Traversable                        (traverse)
import Data.Validation.Semigroup               (toEither)
import DOM.HTML.Indexed.StepValue              (StepValue(..))
import Effect                                  (Effect)
import Effect.Aff                              (Aff)
import Halogen                               as H
import Halogen.Component.ChildPath           as HC
import Halogen.Data.Prism                      (type (<\/>), type (\/))
import Halogen.HTML                          as HH
import Halogen.HTML.Events                   as HE
import Halogen.HTML.Properties               as HP

import Mkpasswd                                     (mkpasswd)
import Mkpasswd.Data.Array                          (modifyAt)
import Mkpasswd.Data.FieldType.Mkpasswd             (FieldType(..))
import Mkpasswd.Data.PasswdPolicy                   (PasswdPolicy, defaultLength, defaultPolicy)
import Mkpasswd.Data.PasswdPolicy.Validation        (validate)
import Mkpasswd.Data.Switch                         (Switch)
import Mkpasswd.Data.Switch                       as Switch
import Mkpasswd.Data.Validation                     (ErrorCode(..))
import Mkpasswd.Halogen.Util                        (classes)
import Mkpasswd.UI.Element                        as UI
import Mkpasswd.UI.Routing                          (RouteHash(..), routeHref)
import Mkpasswd.UI.Components.HeaderNav           as Nav
import Mkpasswd.UI.Components.LabeledInputNumber  as LblInp
import Mkpasswd.UI.Components.PolicyFormRow       as PolRow
import Mkpasswd.UI.Components.SymbolPolicyFormRow as SymRow

newtype PolRowSlot = PRSlot Int

derive newtype instance eqPolRowSlot  :: Eq PolRowSlot
derive newtype instance ordPolRowSlot :: Ord PolRowSlot

type ChildQuery = Nav.Query <\/> LblInp.Query <\/> PolRow.Query <\/> SymRow.Query <\/> Const Void
type ChildSlot  = Unit \/ Unit \/ PolRowSlot \/ Unit \/ Void

cpNav :: HC.ChildPath Nav.Query ChildQuery Unit ChildSlot
cpNav = HC.cp1

cpLblInp :: HC.ChildPath LblInp.Query ChildQuery Unit ChildSlot
cpLblInp = HC.cp2

cpPolRow :: HC.ChildPath PolRow.Query ChildQuery PolRowSlot ChildSlot
cpPolRow = HC.cp3

cpSymRow :: HC.ChildPath SymRow.Query ChildQuery Unit ChildSlot
cpSymRow = HC.cp4

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
    | OnInputLength LblInp.Message a
    | OnInputMinNum PolRow.Message a
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
                    [ (\i -> HH.slot' cpLblInp unit LblInp.ui i $ HE.input OnInputLength)
                        { labelTxt: "Length"
                        , id: "PasswdLength"
                        , disabled: Just false
                        , min: Just $ toNumber 0
                        , max: Just $ toNumber 100
                        , step: Any
                        , value: state.length
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 0) PolRow.ui i $ HE.input OnInputMinNum)
                        { fieldType: DegitsNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 0)
                        , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 0)
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 1) PolRow.ui i $ HE.input OnInputMinNum)
                        { fieldType: UppercaseNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 1)
                        , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 1)
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 2) PolRow.ui i $ HE.input OnInputMinNum)
                        { fieldType: LowercaseNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 2)
                        , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 2)
                        }
                    , (\i -> HH.slot' cpSymRow unit SymRow.ui i $ HE.input OnInputSymNum)
                        { fieldType: SymbolNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 3)
                        , requiredMinNum: fromMaybe "" $ (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 3)
                        , isOpenMulChk: false
                        , allChk: true
                        , chars: (Switch.toSwitch Switch.On) <$> (fromMaybe [] $ (Switch.label >>> _.charSet) <$> (state.policy !! 3))
                        }
                    , if null state.errMsg
                           then HH.text ""
                           else HH.p [ classes [ "h3", "center", "border", "rounded" ] ] [ HH.text $ show state.errMsg ]
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
                                    [ classes [ "level-item", "has-text-centered", "is-size-3" ] ]
                                    [ HH.br_
                                    , HH.text $ fromMaybe "" state.passwd
                                    , HH.br_
                                    ]
                                ]
                            ]
                        , HH.footer
                            [ classes [ "modal-card-foot" ] ]
                            [ HH.a
                                [ classes [ "button", "is-success" ]
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

                    showErr :: ErrorCode -> String
                    showErr = case _ of
                                   OutOfRange   -> "長過ぎます"
                                   ValueMissing -> "入力してください"
                                   EmptyCharSet -> "指定された文字種が空です"
                                   TooShort     -> "長さは文字種ごとの必要最低数の総和よりも大きくしてください"
                                   Unknown      -> "なんかエラーになったんでリロードしてください"

                    mkpasswdE :: Int -> Array PasswdPolicy -> Effect (Either (Array String) String)
                    mkpasswdE l a = note [showErr Unknown] <$> mkpasswd l a

          eval (OnInputLength mi next) = do
              H.modify_ (_ { length = mi })
              pure next

          eval (OnInputMinNum r next) =
             let idx = fieldIdx r.fieldType
              in do
                 s <- H.get
                 let np = modifyAt idx (\w -> (_ { requiredMinNum = r.requiredMinNum }) <$> turn r.isUsed w) s.policy
                 H.modify_ (_ { policy = np })
                 pure next
                 where
                    turn :: forall a. Boolean -> Switch a -> Switch a
                    turn flg = if flg then Switch.on else Switch.off

          eval (OnInputSymNum r next) =
             let idx = fieldIdx r.fieldType
                 pm  = { fieldType: r.fieldType
                       , isUsed: r.isUsed
                       , requiredMinNum: r.requiredMinNum
                       }
              in do
                 s <- H.get
                 H.modify_ (_ { policy = modifyAt idx (map (_ { charSet = mapMaybe Switch.toMaybe r.chars })) s.policy })
                 eval (OnInputMinNum pm next)

          fieldIdx :: FieldType -> Int
          fieldIdx DegitsNum    = 0
          fieldIdx UppercaseNum = 1
          fieldIdx LowercaseNum = 2
          fieldIdx SymbolNum    = 3
