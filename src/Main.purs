module Main where

import Prelude
import Mkpasswd                  (mkpasswd, PasswdPolicy, defaultPolicy)
import Data.Generic.Rep          (class Generic)
import Data.Generic.Rep.Show     (genericShow)
import Data.Maybe                (Maybe(..))
import Data.Either               (Either(..), note)
import Data.Foldable             (oneOf)
import Data.Int                  (fromString)
import Data.String.CodePoints    (length)
import Effect                    (Effect)
import Effect.Aff                (Aff, launchAff_)
import Effect.Class              (liftEffect)
import Effect.Console            (log)
import Halogen                 as H
import Halogen.Aff             as HA
import Halogen.HTML            as HH
import Halogen.HTML.Events     as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver       (runUI)
import Routing.Hash              (matches)
import Routing.Match             (Match, lit, end)

-- | Halogen Utilities
classes :: forall a b. Array String -> HH.IProp ( "class" :: String | b) a
classes = HP.classes <<< map HH.ClassName

style :: forall a b. String -> HH.IProp b a
style = HP.attr $ HH.AttrName "style"

-- | Child component decrertion
type MkpasswdState =
    { policy :: PasswdPolicy
    , passwd :: String
    , errMsg :: String
    }

data MkpasswdQuery a
    = Regenerate a
    | UpdatePolicy FieldType String a

data FieldType
    = PasswdLength
    | DegitsNum
    | UppercaseNum
    | LowercaseNum
    | SymbolNum

derive instance genericFieldType :: Generic FieldType _
instance showFieldType :: Show FieldType where
    show = genericShow

mkpasswdUi :: H.Component HH.HTML MkpasswdQuery Unit Void Aff
mkpasswdUi =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: MkpasswdState
          initialState =
              { policy : defaultPolicy
              , passwd : ""
              , errMsg : ""
              }

          render :: MkpasswdState -> H.ComponentHTML MkpasswdQuery
          render state =
                HH.div
                    [ classes [ "flex-auto" , "flex", "flex-column" ] ]
                    [ HH.h1 [ classes [ "center" ] ] [ HH.text "Mkpasswd" ]
                    , errorView state.errMsg
                    , policyFormRow PasswdLength "ながさ：" $ show state.policy.length
                    , policyFormRow DegitsNum    "すうじ：" $ show state.policy.degit
                    , policyFormRow UppercaseNum "英大字：" $ show state.policy.uppercase
                    , policyFormRow LowercaseNum "英小字：" $ show state.policy.lowercase
                    , policyFormRow SymbolNum    "きごう：" $ show state.policy.symbol
                    , resultView state.passwd
                    , HH.button
                        [ classes [ "self-center", "p1" ]
                        , HE.onClick (HE.input_ Regenerate)
                        ]
                        [ HH.text "Generate new Password" ]
                    ]
          policyFormRow feildType labelTxt currentValue =
              let inpIdStr = show feildType
               in
                  HH.div
                     [ classes [ "flex-none", "clearfix" ] ]
                     [ HH.label
                        [ HP.for inpIdStr
                        , classes [ "col", "col-3", "right-align", "align-baseline", "label" ]
                        ]
                        [ HH.text labelTxt ]
                     , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.id_ inpIdStr
                        , classes [ "col", "col-8", "input" ]
                        , HP.value currentValue
                        , HE.onValueInput $ HE.input (UpdatePolicy feildType)
                        ]
                     ]
          resultView value =
              if length value > 0
                  then HH.p [ classes [ "h3", "center", "border", "rounded" ] ]
                            [ HH.text value ]
                  else HH.text value
          errorView  error =
              if length error > 0
                  then HH.p [ classes [ "h3" , "center" , "border" , "border-red" ] ]
                            [ HH.text error ]
                  else HH.text error

          eval :: MkpasswdQuery ~> H.ComponentDSL MkpasswdState MkpasswdQuery Void Aff
          eval (Regenerate next) = do
              state <- H.get
              newPasswd <- H.liftEffect $ mkpasswd state.policy
              case newPasswd of
                   Right pass -> H.modify_ (_ { errMsg = "", passwd = pass })
                   Left  err  -> H.modify_ (_ { errMsg = err  })
              pure next
          eval (UpdatePolicy feildType value next) =
              let newValue = note (show feildType <> " should be a Number") $ fromString value
               in do
                  state <- H.get
                  case newValue of
                       Right vli -> modifyPolicy feildType state vli
                       Left  err -> H.modify_ (_ { errMsg = err })
                  pure next
          modifyPolicy f s v = do
              let newPolicy =
                    case f of
                       PasswdLength  -> s.policy { length    = v }
                       DegitsNum     -> s.policy { degit     = v }
                       UppercaseNum  -> s.policy { uppercase = v }
                       LowercaseNum  -> s.policy { lowercase = v }
                       SymbolNum     -> s.policy { symbol    = v }
              H.modify_ (_ { errMsg = "", policy = newPolicy })

-- | Routing settings
data RouteHash
    = LinkA
    | LinkB

derive instance genericRouteHash :: Generic RouteHash _
instance showRouteHash :: Show RouteHash where
    show = genericShow

-- | Parent component decrertion
data Slot
    = MkpasswdSlot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State =
    { route  :: RouteHash
    }

data Query a
    = ChangeHash RouteHash a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: State
          initialState =
              { route  : LinkA
              }

          render :: State -> H.ParentHTML Query MkpasswdQuery Slot Aff
          render state =
              HH.main
                [ style "height: 100%"
                , classes [ "flex" , "flex-column" ]
                ]
                [ HH.nav
                    [ classes [ "border", "flex-none", "flex", "justify-center" ] ]
                    [ HH.a
                        [ classes [ "flex-auto", "border", "p1", "center" ]
                        , HP.href "#link/a"
                        ]
                        [ HH.text "つくる" ]
                    , HH.a
                        [ classes [ "flex-auto", "border", "p1", "center" ]
                        , HP.href "#link/b"
                        ]
                        [ HH.text "しまう" ]
                    ]
                , HH.slot MkpasswdSlot mkpasswdUi unit absurd
                ]

          eval :: Query ~> H.ParentDSL State Query MkpasswdQuery Slot Void Aff
          eval (ChangeHash newHash next) = do
              H.modify_ (_ { route = newHash })
              liftEffect $ log $ show newHash
              pure next

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- runUI ui unit body
  liftEffect $ matches menuHash \_ newHash ->
      launchAff_ $ app.query $ H.action $ ChangeHash newHash
      where
            menuHash :: Match RouteHash
            menuHash = lit "link" *> oneOf
                [ LinkA <$ lit "a"
                , LinkB <$ lit "b"
                ] <* end
