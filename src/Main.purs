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

type State =
    { policy :: PasswdPolicy
    , passwd :: String
    , errMsg :: String
    , route  :: RouteType
    }

data FieldType
    = PasswdLength
    | DegitsNum
    | UppercaseNum
    | LowercaseNum
    | SymbolNum

derive instance genericFieldType :: Generic FieldType _
instance showFieldType :: Show FieldType where
    show = genericShow

data Query a
    = ChangeRoute RouteType a
    | Regenerate a
    | UpdatePolicy FieldType String a

classes :: forall a b. Array String -> HH.IProp ( "class" :: String | b) a
classes = HP.classes <<< map HH.ClassName

style :: forall a b. String -> HH.IProp b a
style = HP.attr $ HH.AttrName "style"

headerNav :: forall a b. HH.HTML a b
headerNav =
    HH.nav
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

eval :: Query ~> H.ComponentDSL State Query Void Aff
eval (ChangeRoute newRoute next) = do
    H.modify_ (_ { route = newRoute })
    liftEffect $ log $ show newRoute
    pure next

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
        where
              modifyPolicy f s v = do
                  let newPolicy =
                        case f of
                           PasswdLength  -> s.policy { length    = v }
                           DegitsNum     -> s.policy { degit     = v }
                           UppercaseNum  -> s.policy { uppercase = v }
                           LowercaseNum  -> s.policy { lowercase = v }
                           SymbolNum     -> s.policy { symbol    = v }
                  H.modify_ (_ { errMsg = "", policy = newPolicy })

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver : const Nothing
    }
    where
          initialState :: State
          initialState =
              { policy : defaultPolicy
              , passwd : ""
              , errMsg : ""
              , route  : LinkA
              }

          --render :: forall m. State -> H.ComponentHTML Query () m
          --render :: forall m. State -> H.ComponentHTML (Query Unit)
          render state =
              HH.main
                [ style "height: 100%"
                , classes [ "flex" , "flex-column" ]
                ]
                [ headerNav
                , HH.h1 [ classes [ "center" ] ] [ HH.text "Mkpasswd" ]
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
                where
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

data RouteType
    = LinkA
    | LinkB

derive instance genericRouteType :: Generic RouteType _
instance showRouteType :: Show RouteType where
    show = genericShow

menuRoute :: Match RouteType
menuRoute = lit "link" *> oneOf
    [ LinkA <$ lit "a"
    , LinkB <$ lit "b"
    ] <* end

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- runUI ui unit body
  liftEffect $ matches menuRoute \_ newHash ->
      launchAff_ $ app.query $ H.action $ ChangeRoute newHash
