module Halogen.Typewriter where

import Prelude hiding (div)

import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Lens (view, (%=), (.=))
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
import Halogen.Typewriter.Lens (cursorHidden, deleteDelay, mode, outputText, pauseDelay, typeDelay, words)

-- TODO: Add typing delay jitter.

-- | Configuration to initialize 'typewriter'.
type Input =
  { -- | Words for 'typewriter' to type.
    words :: List String
  -- | Delay after typing a letter.
  , typeDelay :: Milliseconds
  -- | Delay after deleting a letter.
  , deleteDelay :: Milliseconds
  -- | Delay a word is fully typed out.
  , pauseDelay :: Milliseconds
  -- | Delay in between showing/hiding the cursor.
  , cursorDelay :: Milliseconds
  -- | The cursor to display. Use 'Nothing' to hide it.
  , cursor :: Maybe Char
  }

-- | Internal state for 'typewriter'.
type State =
  { -- | Words for 'typewriter' to type.
    words :: List String
  -- | The currently displayed text.
  , outputText :: String
  -- | Delay after typing a letter.
  , typeDelay :: Milliseconds
  -- | Delay after deleting a letter.
  , deleteDelay :: Milliseconds
  -- | Delay a word is fully typed out.
  , pauseDelay :: Milliseconds
  -- | Delay in between showing/hiding the cursor.
  , cursorDelay :: Milliseconds
  -- | Whether the typewriter should be typing or deleting.
  , mode :: Mode
  -- | The cursor to display. Use 'Nothing' to hide it.
  , cursor :: Maybe Char
  -- | Current cursor visibility. Used for the blinking effect.
  , cursorHidden :: Boolean
  }

-- | Current typewriter mode.
data Mode
  -- | The typewriter is currently typing letters.
  = Typing
  -- | The typewriter is currently deleting letters.
  | Deleting

-- | Internal actions for 'typewriter'.
data Action
  -- | Start the 'typewriter'. This should only be invoked internally.
  -- | Running this on your own has unintended consequences.
  = Initialize
  -- | Update the typewriter's state.
  | Update
  -- | Toggle the cursor's visibility.
  | ToggleCursor

-- | Default typewriter 'Input' parameters.
defaultTypewriter :: List String -> Input
defaultTypewriter words =
  { words
  , typeDelay: Milliseconds $ toNumber 140
  , deleteDelay: Milliseconds $ toNumber 100
  , pauseDelay: Milliseconds $ toNumber 700
  , cursorDelay: Milliseconds $ toNumber 900
  , cursor: Just '|'
  }

-- | Possible outputs for 'typewriter'.
data Output =
  -- | Emitted after 'State.words' becomes empty.
  -- | If 'State.words' is an infinite list, this will never be emitted.
  Finished

-- | Typewriter component. For more info on how to use this as a child component, see here:
-- | https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html
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
              -- Gets the next letter of the word.
              case charAt (length state.outputText) word of
                Nothing -> do
                  -- Delete the current word from state.words.
                  words %= fromMaybe mempty <<< tail
                  mode .= Deleting
                  sleep pauseDelay
                Just letter -> do
                  sleep typeDelay
                  -- Add the next letter to outputText.
                  outputText %= (_ <> singleton letter)
            Deleting ->
              -- When outputText is empty, start typing.
              if null state.outputText then mode .= Typing
              else do
                sleep deleteDelay
                -- Remove the last letter of outputText.
                -- It's unfortunate the 'init' function doesn't exist for strings!
                outputText .= (splitAt (length state.outputText - 1) state.outputText).before
      handleAction Update
