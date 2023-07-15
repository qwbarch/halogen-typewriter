module Main where

import Prelude hiding (div)

import CSS (maxHeight, textWhitespace, vh, whitespacePreWrap)
import CSS.Overflow (overflow, overflowAuto)
import Data.Array (singleton)
import Data.List.Lazy (cycle, fromFoldable, repeat, replicate)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), Component, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (IProp, code_, div, h1, h2, h3, p_, pre, section, slot_, span, span_, text)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (class_)
import Halogen.Typewriter (defaultTypewriter, typewriter)
import Halogen.VDom.Driver (runUI)
import Substitute (normalize)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

css :: ∀ r i. String -> IProp (class :: String | r) i
css = class_ <<< ClassName

component :: ∀ q i o m. MonadAff m => Component q i o m
component =
  mkComponent
    { initialState: const unit
    , render
    , eval: mkEval defaultEval
    }
  where
  bold = span [ css "has-text-weight-bold" ] <<< singleton <<< text

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

  singleWordExample =
    let
      input = defaultTypewriter { words = repeat "Hello world!" }
    in
      example
        { title: "Single word"
        , description:
            span_
              [ bold "words"
              , span_ [ text " is defined as a lazy list. Once the list is finished, it'll stop typing." ]
              , p_
                  [ text "Simply provide an infinite list with "
                  , bold "repeat"
                  , text " and it'll never stop!"
                  ]
              ]
        , code: normalize
            """
            defaultTypewriter { words = repeat "Hello world!" }
            """
        , typewriter: Just $ slot_ (Proxy :: Proxy "single-word") 1 typewriter input
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
              , bold "fromFoldable"
              , text " to create the list, and "
              , bold "cycle"
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
              }
            """
        , typewriter:
            Just $
              span_
                [ text "My name is "
                , slot_ (Proxy :: Proxy "multiple-words") 2 typewriter input
                ]
        }

  finiteRunsExample =
    let
      input = defaultTypewriter { words = replicate 2 "I'm getting tired of typing this..." }
    in
      example
        { title: "Finite runs"
        , description:
            span_
              [ text "You probably get it by now. The number of times a word is typed is based on what "
              , bold "words"
              , text " contains."
              ]
        , code: normalize
            """
            defaultTypewriter { words = replicate 2 "I'm getting tired of typing this..." }
            """
        , typewriter: Just $ slot_ (Proxy :: Proxy "finite-runs") 3 typewriter input
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
                      , singleWordExample
                      , multipleWordsExample
                      , finiteRunsExample
                      ]
                  ]
              ]
          ]
      ]
