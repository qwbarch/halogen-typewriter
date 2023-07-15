module Main where

import Prelude hiding (div)

import CSS (marginLeft, maxHeight, minWidth, nil, padding, paddingBottom, paddingLeft, paddingTop, rem, textWhitespace, vh, whitespacePreWrap)
import CSS.Common (none)
import CSS.Overflow (overflow, overflowAuto)
import Css (css)
import Data.Lens ((.~))
import Data.List.Lazy (Step(..), cycle, fromFoldable, repeat, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (Component, Slot, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (code, code_, div, div_, footer, h1, h2, h3, header, p, p_, pre, pre_, section, slot_, span, span_, text)
import Halogen.HTML.CSS (style)
import Halogen.Typewriter (Output, defaultTypewriter, typewriter)
import Halogen.VDom.Driver (runUI)
import Substitute (normalize)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

component :: ∀ q i o m. MonadAff m => Component q i o m
component =
  mkComponent
    { initialState: const unit
    , render
    , eval: mkEval defaultEval
    }
  where
  subtitle =
    defaultTypewriter
      { words = cycle $ fromFoldable
          [ "Is a simple typewriter."
          , "Is made in Halogen."
          , "Is made in PureScript."
          , "Is a tiny library."
          ]
      , typeDelay = Milliseconds 60.0
      , deleteDelay = Milliseconds 20.0
      }

  quickstartExample =
    example
      { title: "Quickstart"
      , description: text "Minimal example to get started:"
      , typewriter: Nothing
      , code: normalize
          """
          main :: Effect Unit
          main = runHalogenAff do
            body <- awaitBody
            runUI component unit body

          component :: ∀ q i o m. MonadAff m => Component q i o m
          component =
            mkComponent
              { initialState: const unit
              , render
              , eval: mkEval defaultEval
              }
            where
            render _ = div_ [ slot_ (Proxy :: Proxy "typewriter") 0 typewriter input ]
            input = defaultTypewriter { words = repeat "Hello world!" }
          """
      }

  simpleExample =
    let
      input = defaultTypewriter { words = repeat "Hello world!" }
    in
      example
        { title: "Simple"
        , description:
            div_
              [ span [ css "has-text-weight-bold" ] [ text "words" ]
              , text " is defined as a lazy list. Simply provide an infinite list to make the typewriter never stop!"
              ]
        , code: normalize
            """
            defaultTypewriter { words = repeat "Hello world!" }
            """
        , typewriter: Just $ slot_ (Proxy :: Proxy "simple") 1 typewriter input
        }

  multipleWordsExample =
    let
      input =
        defaultTypewriter
          { words = cycle $ fromFoldable $ map (_ <> ".") $
              [ "John"
              , "Olivia"
              , "Liam"
              , "Emma"
              , "Noah"
              , "Charlotte"
              , "Oliver"
              , "Amelia"
              ]
          }
    in
      example
        { title: "Multiple words"
        , description:
            span_
              [ text "Use "
              , span [ css "has-text-weight-bold" ] [ text "fromFoldable" ]
              , text " to create the list, and "
              , span [ css "has-text-weight-bold" ] [ text "cycle" ]
              , text " to repeat it indefinitely."
              ]
        , code: normalize
            """
            defaultTypewriter
              { words = cycle $ fromFoldable $ map (_ <> ".") $
                  [ "John"
                  , "Olivia"
                  , "Liam"
                  , "Emma"
                  , "Noah"
                  , "Charlotte"
                  , "Oliver"
                  , "Amelia"
                  ]
            """
        , typewriter:
            Just $
              span_
                [ text "My name is "
                , slot_ (Proxy :: Proxy "multiple-words") 2 typewriter input
                ]
        }

  example template =
    div
      [ css "container pb-5" ]
      [ h3 [ css "has-text-weight-semibold is-size-4" ] [ text template.title ]
      , p_ [ template.description ]
      , div
          [ css "container pb-2 is-flex is-align-items-center" ]
          case template.typewriter of
            Just typewriter ->
              [ span [ css "pr-2 has-text-weight-bold is-size-5" ] [ text ">" ], typewriter ]
            Nothing -> []

      , div
          [ css "card"
          , style $ do
              overflow overflowAuto
              maxHeight $ vh 40.0
          ]
          [ pre
              [ style $ textWhitespace whitespacePreWrap ]
              [ code_ [ text template.code ] ]
          ]
      ]

  render _ =
    div [ css "is-flex is-justify-content-center is-align-items-center" ]
      [ section [ css "section" ]
          [ div [ css "card" ]
              [ section
                  [ css "section"
                  ]
                  [ div [ css "card-content" ]
                      [ h1 [ css "title" ] [ text "Halogen Typewriter" ]
                      , h2
                          [ css "subtitle" ]
                          [ slot_ (Proxy :: Proxy "subtitle") 0 typewriter subtitle
                          ]
                      , quickstartExample
                      , simpleExample
                      , multipleWordsExample
                      ]
                  ]
              ]
          ]
      ]
