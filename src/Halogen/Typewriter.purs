module Halogen.Typewriter where

import Prelude

import Control.Monad.State (get)
import Data.Int (toNumber)
import Data.Lens (view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.List.Lazy (List, head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null, splitAt)
import Data.String.CodeUnits (charAt, length, singleton)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (Component, defaultEval, mkComponent, mkEval, raise)
import Halogen.HTML (text)
import Type.Prelude (Proxy(..))

type State =
  { words :: List String
  , outputText :: String
  , typeDelay :: Milliseconds
  , deleteDelay :: Milliseconds
  , mode :: Mode
  }

data Mode = Typing | Deleting

data Action = UpdateState

type Input =
  { words :: List String
  , typeDelay :: Milliseconds
  , deleteDelay :: Milliseconds
  }

defaultTypewriter :: List String -> Input
defaultTypewriter words =
  { words
  , typeDelay: Milliseconds $ toNumber 140
  , deleteDelay: Milliseconds $ toNumber 100
  }

data Output = Finished

typewriter :: ∀ q m. MonadAff m => Component q Input Output m
typewriter = mkComponent { initialState, render, eval }
  where
  initialState input =
    { words: input.words
    , outputText: mempty
    , typeDelay: input.typeDelay
    , deleteDelay: input.deleteDelay
    , mode: Typing
    }
  eval = mkEval (defaultEval { handleAction = handleAction, initialize = Just UpdateState })

  -- Lenses for 'State' properties.
  -- Unfortunately PureScript doesn't have a template-haskell equivalent, so these need to be manually defined.
  words = prop (Proxy :: Proxy "words")
  outputText = prop (Proxy :: Proxy "outputText")
  mode = prop (Proxy :: Proxy "mode")
  typeDelay = prop (Proxy :: Proxy "typeDelay")
  deleteDelay = prop (Proxy :: Proxy "deleteDelay")

  render state = text state.outputText

  handleAction = case _ of
    UpdateState -> do
      state :: State <- get
      let sleep = liftAff <<< delay <<< flip view state
      case head state.words of
        Nothing -> raise Finished
        Just word -> do
          case state.mode of
            Typing ->
              case charAt (length state.outputText) word of
                Nothing -> do
                  mode .= Deleting
                  words %= fromMaybe mempty <<< tail
                Just letter -> do
                  sleep typeDelay
                  outputText %= (_ <> singleton letter)
            Deleting ->
              if null state.outputText then mode .= Typing
              else do
                sleep deleteDelay
                outputText .= (splitAt (length state.outputText - 1) state.outputText).before
      handleAction UpdateState
