module Halogen.Typewriter where

import Prelude hiding (div)

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Lens (view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.List.Lazy (List, head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null, splitAt)
import Data.String.CodeUnits (charAt, length, singleton)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen (Component, defaultEval, mkComponent, mkEval, raise, subscribe)
import Halogen.HTML (span_, text)
import Halogen.Subscription (create, notify)
import Type.Prelude (Proxy(..))

type State =
  { words :: List String
  , outputText :: String
  , typeDelay :: Milliseconds
  , deleteDelay :: Milliseconds
  , pauseDelay :: Milliseconds
  , cursorDelay :: Milliseconds
  , mode :: Mode
  , cursor :: Maybe Char
  , cursorHidden :: Boolean
  }

data Mode = Typing | Deleting

data Action = Initialize | Update | ToggleCursor

type Input =
  { words :: List String
  , typeDelay :: Milliseconds
  , deleteDelay :: Milliseconds
  , pauseDelay :: Milliseconds
  , cursorDelay :: Milliseconds
  , cursor :: Maybe Char
  }

defaultTypewriter :: List String -> Input
defaultTypewriter words =
  { words
  , typeDelay: Milliseconds $ toNumber 140
  , deleteDelay: Milliseconds $ toNumber 100
  , pauseDelay: Milliseconds $ toNumber 700
  , cursorDelay: Milliseconds $ toNumber 900
  , cursor: Just '|'
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
    , pauseDelay: input.pauseDelay
    , cursorDelay: input.cursorDelay
    , mode: Typing
    , cursor: input.cursor
    , cursorHidden: false
    }
  eval = mkEval (defaultEval { handleAction = handleAction, initialize = Just Initialize })

  -- Lenses for 'State' properties.
  -- Unfortunately PureScript doesn't have a template-haskell equivalent, so these need to be manually defined.
  words = prop (Proxy :: Proxy "words")
  outputText = prop (Proxy :: Proxy "outputText")
  mode = prop (Proxy :: Proxy "mode")
  typeDelay = prop (Proxy :: Proxy "typeDelay")
  deleteDelay = prop (Proxy :: Proxy "deleteDelay")
  pauseDelay = prop (Proxy :: Proxy "pauseDelay")
  cursorHidden = prop (Proxy :: Proxy "cursorHidden")

  render state = span_
    [ text state.outputText
    , span_ [ text $ if state.cursorHidden then "" else foldMap singleton state.cursor ]
    ]

  handleAction action = get >>= \state -> case action of
    Initialize -> do
      { emitter, listener } <- liftEffect create
      void $ subscribe emitter
      void $ liftAff $ forkAff $ forever do
        delay state.cursorDelay
        liftEffect $ notify listener ToggleCursor
      handleAction Update
    ToggleCursor -> cursorHidden %= not
    Update -> do
      let sleep = liftAff <<< delay <<< flip view state
      case head state.words of
        Nothing -> raise Finished
        Just word -> do
          case state.mode of
            Typing ->
              case charAt (length state.outputText) word of
                Nothing -> do
                  words %= fromMaybe mempty <<< tail
                  mode .= Deleting
                  sleep pauseDelay
                Just letter -> do
                  sleep typeDelay
                  outputText %= (_ <> singleton letter)
            Deleting ->
              if null state.outputText then mode .= Typing
              else do
                sleep deleteDelay
                outputText .= (splitAt (length state.outputText - 1) state.outputText).before
      handleAction Update
