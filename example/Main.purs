module Main where

import Prelude hiding (div)

import Css (css)
import Data.Lens ((.~))
import Data.List.Lazy (cycle, fromFoldable)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (div, h1, h2, section, slot_, text)
import Halogen.Typewriter (defaultTypewriter, typewriter)
import Halogen.Typewriter.Lens (words)
import Halogen.VDom.Driver (runUI)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

data Action = Increment | Decrement

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
      # words .~ cycle (fromFoldable [ "Is a typewriter made in Halogen." ])
  render _ =
    div [ css "is-flex is-justify-content-center is-align-items-center" ]
      [ section
          [ css "section"
          ]
          [ h1 [ css "title has-text-centered" ] [ text "Halogen Typewriter" ]
          , h2
              [ css "subtitle has-text-centered" ]
              [ slot_ (Proxy :: Proxy "subtitle") 0 typewriter subtitle
              ]
          ]
      ]
