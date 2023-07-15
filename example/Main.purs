module Main where

import Prelude hiding (div)

import CSS (minWidth, rem)
import Css (css)
import Data.Lens ((.~))
import Data.List.Lazy (Step(..), cycle, fromFoldable, repeat, (:))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (Component, Slot, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (code, div, footer, h1, h2, h3, header, p_, pre_, section, slot_, span, span_, text)
import Halogen.HTML.CSS (style)
import Halogen.Typewriter (Output, defaultTypewriter, typewriter)
import Halogen.VDom.Driver (runUI)
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
          [ "Is a typewriter."
          , "Is made in Halogen."
          , "Is made in PureScript."
          , "Is a tiny library."
          ]
      }

  minimalExample =
    { title: "Minimal"
    , description:
        p_
          [ span [ css "has-text-weight-bold" ] [ text "words" ]
          , text " is defined as a lazy list. Simply provide an infinite list to make the typewriter never stop!"
          ]
    , input: defaultTypewriter { words = repeat "Hello world!" }
    , code:
        """
        defaultTypewriter { words = repeat "Hello world!" }
        """
    }

  example template =
    div
      [ css "container" ]
      [ h3 [ css "has-text-weight-semibold is-size-4" ] [ text template.title ]
      , p_ [ template.description ]
      , div [ css "container pb-2 is-flex is-align-items-center" ] [ span [ css "pr-2 has-text-weight-bold is-size-5" ] [ text ">" ], template.typewriter ]
      , div [ css "card" ]
          [ div [ css "notification" ] [ text template.code ]
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
                      , example
                          { title: minimalExample.title
                          , description: minimalExample.description
                          , code: minimalExample.code
                          , typewriter: slot_ (Proxy :: Proxy "minimal") 1 typewriter minimalExample.input
                          }
                      ]
                  ]
              ]
          ]
      ]
