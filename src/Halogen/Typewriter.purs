module Halogen.Typewriter where

import Prelude hiding (div)

import CSS (opacity)
import Control.Monad.State (get, modify)
import Data.Foldable (fold)
import Data.Lens (view, (%=), (.=), (<>=))
import Data.List.Lazy (List, head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (null, take)
import Data.String.CodeUnits (charAt, length, singleton)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Random (randomRange)
import Halogen (ClassName(..), Component, ComponentHTML, defaultEval, mkComponent, mkEval, raise, subscribe)
import Halogen.HTML (span, text)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties.ARIA as Aria
import Halogen.Subscription (create, notify)
import Halogen.Typewriter.Lens (cursorDelay, cursorHidden, deleteDelay, mode, outputText, pauseDelay, typeDelay, words)

-- | Configuration to initialize 'typewriter'.
type Input m =
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
  -- | The cursor to display.
  , cursor :: ComponentHTML Action () m
  -- | Used to make the typing/deleting speed a bit more randomized.
  -- | The result of this function is multiplied by the typing delay.
  , jitter :: Effect Number
  }

-- | Internal state for 'typewriter'.
type State m =
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
  -- | The cursor to display.
  , cursor :: ComponentHTML Action () m
  -- | Current cursor visibility. Used for the blinking effect.
  , cursorHidden :: Boolean
  -- | Used to make the typing/deleting speed a bit more randomized.
  -- | The result of this function is multiplied by the typing delay.
  , jitter :: Effect Number
  -- | Whether the typewriter is running or not.
  , running :: Boolean
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
  | UpdateState
  -- | Update the typewriter's cursor visibility.
  | UpdateCursor

-- | Default typewriter 'Input' parameters.
defaultTypewriter :: ∀ m. Input m
defaultTypewriter =
  { words: mempty
  , typeDelay: Milliseconds 100.0
  , deleteDelay: Milliseconds 40.0
  , pauseDelay: Milliseconds 900.0
  , cursorDelay: Milliseconds 700.0
  , cursor: text "|"
  , jitter: randomRange 0.9 1.1
  }

-- | Possible outputs for 'typewriter'.
data Output =
  -- | Emitted after 'State.words' becomes empty.
  -- | If 'State.words' is an infinite list, this will never be emitted.
  Finished

-- | Typewriter component. For more info on how to use this as a child component, see here:
-- | https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html
typewriter :: ∀ q m. MonadAff m => Component q (Input m) Output m
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
    , cursorHidden: true
    , jitter: input.jitter
    , running: true
    }
  eval = mkEval defaultEval { handleAction = handleAction, initialize = Just Initialize }

  -- The defined class names do nothing by default.
  -- These are for the user's convenience, if they want to change styles for the respective classes.
  render state =
    span
      [ class_ (ClassName "typewriter"), Aria.label (fold $ head state.words) ]
      [ span
          [ class_ (ClassName "typewriter-text"), Aria.hidden "true" ]
          [ text state.outputText ]
      , span
          [ class_ (ClassName "typewriter-cursor")
          , Aria.hidden "true"
          , style $ when (state.cursorHidden) $ opacity 0.0
          ]
          [ state.cursor ]
      ]

  handleAction action = get >>= \state ->
    let
      sleep modifyDelay = liftAff <<< delay <<< Milliseconds <<< modifyDelay <<< unwrap <<< flip view state
    in
      case action of
        Initialize -> do
          { emitter, listener } <- liftEffect create
          let forkDispatch = void <<< liftAff <<< forkAff <<< liftEffect <<< notify listener
          void $ subscribe emitter
          forkDispatch UpdateCursor
          forkDispatch UpdateState
        UpdateCursor ->
          if state.running then do
            cursorHidden %= not
            sleep identity cursorDelay
            handleAction UpdateCursor
          else do
            cursorHidden .= true
            raise Finished
        UpdateState ->
          case head state.words of
            Nothing -> void $ modify $ _ { running = false, cursorHidden = true }
            Just word -> do
              case state.mode of
                Typing ->
                  -- Gets the next letter of the word.
                  case charAt (length state.outputText) word of
                    Nothing -> do
                      -- Delete the current word from state.words.
                      words %= fromMaybe mempty <<< tail
                      mode .= Deleting
                      sleep identity pauseDelay
                    Just letter -> do
                      coefficient <- liftEffect $ state.jitter
                      sleep (_ * coefficient) typeDelay
                      -- Add the next letter to outputText.
                      outputText <>= singleton letter
                Deleting ->
                  -- When outputText is empty, start typing.
                  if null state.outputText then mode .= Typing
                  else do
                    sleep identity deleteDelay
                    -- Remove the last letter of outputText.
                    -- It's unfortunate the 'init' function doesn't exist for strings!
                    outputText .= take (length state.outputText - 1) state.outputText
              handleAction UpdateState
