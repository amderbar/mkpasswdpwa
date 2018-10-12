module Mkpasswd.UI.Pages.Mkpasswd where

import Prelude
import Data.Array                              ((!!), mapMaybe)
import Data.Const                              (Const)
import Data.Either                             (Either(..), note)
import Data.Int                                (toNumber)
import Data.Maybe                              (Maybe(..), fromMaybe)
import Data.Tuple                              (uncurry)
import Data.Validation.Semigroup               (toEither)
import DOM.HTML.Indexed.StepValue              (StepValue(..))
import Effect                                  (Effect)
import Effect.Aff                              (Aff)
import Halogen                               as H
import Halogen.Component.ChildPath           as HC
import Halogen.Data.Prism                      (type (<\/>), type (\/))
import Halogen.HTML                          as HH
import Halogen.HTML.Events                   as HE
--import Halogen.HTML.Properties               as HP

import Mkpasswd                                     (mkpasswd)
import Mkpasswd.Data.Array                          (modifyAt)
import Mkpasswd.Data.FieldType.Mkpasswd             (FieldType(..), labelTxt)
import Mkpasswd.Data.PasswdPolicy                   (PasswdPolicy, defaultLength, defaultPolicy)
import Mkpasswd.Data.PasswdPolicy.Validation        (validate)
import Mkpasswd.Data.Switch                         (Switch)
import Mkpasswd.Data.Switch                       as Switch
import Mkpasswd.Data.Validation                     (ErrorCode(..))
import Mkpasswd.Halogen.Util                        (classes)
import Mkpasswd.UI.Components.LabeledInputNumber  as LblInp
import Mkpasswd.UI.Components.PolicyFormRow       as PolRow
import Mkpasswd.UI.Components.SymbolPolicyFormRow as SymRow

newtype PolRowSlot = PRSlot Int

derive newtype instance eqPolRowSlot  :: Eq PolRowSlot
derive newtype instance ordPolRowSlot :: Ord PolRowSlot

type ChildQuery = LblInp.Query <\/> PolRow.Query <\/> SymRow.Query <\/> Const Void
type ChildSlot  = Unit \/ PolRowSlot \/ Unit \/ Void

cpLblInp :: HC.ChildPath LblInp.Query ChildQuery Unit ChildSlot
cpLblInp = HC.cp1

cpPolRow :: HC.ChildPath PolRow.Query ChildQuery PolRowSlot ChildSlot
cpPolRow = HC.cp2

cpSymRow :: HC.ChildPath SymRow.Query ChildQuery Unit ChildSlot
cpSymRow = HC.cp3

type Input =
    Unit

type Message =
    Maybe String

type State =
    { length :: Int
    , policy :: Array (Switch PasswdPolicy)
    , passwd :: Maybe String
    , errMsg :: Maybe (Array String)
    }

data Query a
    = Regenerate a
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
          initialState = const
              { length : defaultLength
              , policy : (Switch.toSwitch Switch.On) <$> defaultPolicy
              , passwd : Nothing
              , errMsg : Nothing
              }

          render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
          render state =
              HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1_  [ HH.text "Mkpasswd" ]
                    , (\i -> HH.slot' cpLblInp unit LblInp.ui i $ HE.input OnInputLength)
                        { labelTxt: "ながさ"
                        , id: "PasswdLength"
                        , disabled: Just false
                        , min: Just $ toNumber 0
                        , max: Just $ toNumber 100
                        , step: Any
                        , value: Just state.length
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 0) PolRow.ui i $ HE.input OnInputMinNum)
                        { fieldType: DegitsNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 0)
                        , requiredMinNum: (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 0)
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 1) PolRow.ui i $ HE.input OnInputMinNum)
                        { fieldType: UppercaseNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 1)
                        , requiredMinNum: (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 1)
                        }
                    , (\i -> HH.slot' cpPolRow (PRSlot 2) PolRow.ui i $ HE.input OnInputMinNum)
                        { fieldType: LowercaseNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 2)
                        , requiredMinNum: (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 2)
                        }
                    , (\i -> HH.slot' cpSymRow unit SymRow.ui i $ HE.input OnInputSymNum)
                        { fieldType: SymbolNum
                        , isUsed: fromMaybe false $ Switch.isOn <$> (state.policy !! 3)
                        , requiredMinNum: (Switch.label >>> _.requiredMinNum) <$> (state.policy !! 3)
                        , isOpenMulChk: false
                        , allChk: true
                        , chars: (Switch.toSwitch Switch.On) <$> (fromMaybe [] $ (Switch.label >>> _.charSet) <$> (state.policy !! 3))
                        }
                    , case state.errMsg of
                           Nothing -> HH.text ""
                           Just ae -> HH.p [ classes [ "h3", "center", "border", "rounded" ] ] [ HH.text $ show ae ]
                    , case state.passwd of
                           Nothing -> HH.text ""
                           Just v  -> HH.p [ classes [ "h3", "center", "border", "rounded" ] ] [ HH.text v ]
                    , HH.button
                        [ classes [ "flex-none", "self-center", "p1" ]
                        , HE.onClick (HE.input_ Regenerate)
                        ]
                        [ HH.text "つくる" ]
                    ]

          eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
          eval (Regenerate next) = do
              s <- H.get
              ep <- H.liftEffect $
                  let vp = toEither $ validate s.length (mapMaybe Switch.toMaybe s.policy)
                   in case vp of
                           Right prm -> uncurry mkpasswdE prm
                           Left  err -> pure $ Left err
              case ep of
                   Right p -> H.modify_ (_ { errMsg = Nothing, passwd = Just p })
                   Left  e -> H.modify_ (_ { errMsg = Just (showErr <$> e) })
              pure next
              where
                    mkpasswdE :: Int -> Array PasswdPolicy -> Effect (Either (Array ErrorCode) String)
                    mkpasswdE l a = note [Unknown] <$> mkpasswd l a

                    showErr :: ErrorCode -> String
                    showErr OutOfRange   = "長過ぎます"
                    showErr ValueMissing = "入力してください"
                    showErr EmptyCharSet = "指定された文字種が空です"
                    showErr TooShort     = "長さは文字種ごとの必要最低数の総和よりも大きくしてください"
                    showErr Unknown      = "なんかエラーになったんでリロードしてください"

          eval (OnInputLength mi next) =
             let el = note ("長さには数値を入れてください") mi
              in do
                 s <- H.get
                 case el of
                      Right l -> H.modify_ (_ { errMsg = Nothing, length = l })
                      Left  e -> H.modify_ (_ { errMsg = Just [e] })
                 pure next

          eval (OnInputMinNum r next) =
             let fieldName = labelTxt r.fieldType
                 errMsg    = fieldName <> "を含める数には数値を入れてください"
                 idx       = fieldIdx r.fieldType
              in do
                 s <- H.get
                 case r.requiredMinNum of
                      Just v  ->
                           let np = modifyAt idx (\w -> (_ { requiredMinNum = v }) <$> turn r.isUsed w) s.policy
                           in H.modify_ (_ { errMsg = Nothing, policy = np })
                      Nothing -> H.modify_ (_ { errMsg = Just [errMsg] })
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
