module Main where

import Prelude

import Data.List.Lazy (cycle, fromFoldable)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (slot_)
import Halogen.Typewriter (defaultTypewriter, typewriter)
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
  render _ = slot_ (Proxy :: Proxy "typewriter") 0 typewriter typewriterInput
  typewriterInput =
    defaultTypewriter $ cycle $ fromFoldable [ "hello", "world!" ]
