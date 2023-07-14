module Halogen.Typewriter where

import Prelude

import Data.List.Lazy (List)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (text)

type State =
  { words :: List String
  }

data Action = Foo | Bar

typewriter :: ∀ q i o m. Component q i o m
typewriter = mkComponent { initialState, render, eval }
  where
  initialState :: i -> State
  initialState _ =
    { words: mempty
    }
  eval = mkEval (defaultEval { handleAction = handleAction })
  handleAction = case _ of
    Foo -> pure unit
    Bar -> pure unit
  render _ = text ""
