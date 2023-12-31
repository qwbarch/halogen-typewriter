module Main where

import Prelude hiding (div)

import CSS (maxHeight, textWhitespace, vh, whitespacePreWrap)
import CSS.Overflow (overflow, overflowAuto)
import Data.Array (singleton)
import Data.List.Lazy (cycle, fold, fromFoldable, intercalate, repeat, replicate)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..), fromDuration)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (randomRange)
import Halogen (ClassName(..), Component, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (IProp, code, code_, div, div_, h1, h2, h3, p, p_, pre, section, slot_, span, span_, text)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties.ARIA as Aria
import Halogen.Typewriter (Mode(..), defaultTypewriter, typewriter)
import Halogen.VDom.Driver (runUI)
import Substitute (normalize)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = runHalogenAff $ runUI component unit =<< awaitBody

-- | Run the HighlightJS syntax highlighter.
foreign import highlightAll :: Effect Unit

css :: ∀ r i. String -> IProp (class :: String | r) i
css = class_ <<< ClassName

data Action = Initialize

component :: ∀ q i o m. MonadAff m => Component q i o m
component =
  mkComponent
    { initialState: const unit
    , render
    , eval: mkEval defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  handleAction = case _ of
    Initialize -> liftEffect highlightAll

  bold = span [ css "has-text-weight-bold" ] <<< singleton <<< text
  subtitle =
    defaultTypewriter
      { words = cycle $ fromFoldable
          [ "a simple typewriter."
          , "made with Halogen."
          , "made in PureScript."
          , "a tiny library."
          ]
      , typeDelay = Milliseconds 60.0
      , deleteDelay = Milliseconds 20.0
      }
  typewriterHtml = slot_ (Proxy :: Proxy "typewriter") unit typewriter

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
            -- See here for more information on how to use slot:
            -- https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html
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
        , typewriter: Just $ typewriterHtml input
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
        , typewriter: Just $ span_ [ text "My name is ", typewriterHtml input ]
        }

  finiteRunsExample =
    let
      input = defaultTypewriter { words = replicate 2 "I will only type this sentence twice." }
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
            defaultTypewriter { words = replicate 2 "I will only type this sentence twice." }
            """
        , typewriter: Just $ typewriterHtml input
        }

  typingSpeedExample =
    let
      input =
        defaultTypewriter
          { words = repeat "I take my time when I type..."
          , typeDelay = Milliseconds 400.0
          , deleteDelay = Milliseconds 100.0
          , pauseDelay = fromDuration $ Seconds 1.0
          }
    in
      example
        { title: "Typing speed"
        , description: text "You can adjust the typing, deleting, and pause (in between words) delay."
        , code: normalize
            """
            defaultTypewriter
              { words = repeat "I take my time when I type..."
              , typeDelay = Milliseconds 400.0
              , deleteDelay = Milliseconds 100.0
              , pauseDelay = fromDuration $ Seconds 1.0
              }
            """
        , typewriter: Just $ typewriterHtml input
        }

  jitterExample =
    let
      input =
        defaultTypewriter
          { words = repeat "The Cow King is a Hell Bovine monarch that is associated with The Secret Cow Level."
          , jitter = randomRange 0.5 4.0
          }
    in
      example
        { title: "Jitter"
        , description:
            span_
              [ text "A jitter range of "
              , bold "(0.5, 4.0)"
              , text " means the typewriter waits for "
              , code_ [ text "typeDelay * randomRange(0.5, 4.0)" ]
              , text " in between letters."
              ]
        , code: normalize
            """
            defaultTypewriter
              { words = repeat "The Cow King is a Hell Bovine monarch that is associated with The Secret Cow Level."
              , jitter = randomRange 0.5 4.0
              }
            """
        , typewriter: Just $ typewriterHtml input
        }

  emptyCursorExample =
    let
      input =
        defaultTypewriter
          { words = cycle $ fromFoldable
              [ "Knowledge is power."
              , "Life is like a box of chocolates. You never know what you're gonna get."
              , "Life is like riding a bicycle. To keep your balance, you must keep moving."
              , "May the Force be with you."
              ]
          , cursor = text ""
          }
    in
      example
        { title: "Empty cursor"
        , description: text "Simply provide empty HTML to hide the cursor."
        , code: normalize
            """
            defaultTypewriter
              { words = cycle $ fromFoldable
                  [ "Knowledge is power."
                  , "Life is like a box of chocolates. You never know what you're gonna get."
                  , "Life is like riding a bicycle. To keep your balance, you must keep moving."
                  , "May the Force be with you."
                  ]
              , cursor = text ""
              }
            """
        , typewriter: Just $ typewriterHtml input
        }

  deletingExample =
    let
      input =
        defaultTypewriter
          { words = repeat "Wisdom is the offspring of suffering and time."
          , initialMode = Deleting
          }
    in
      example
        { title: "Backspace initially"
        , description:
            text $ intercalate " "
              [ "Instead of typing the initial word, you can have the typewriter delete it instead."
              , "If you don't notice a difference, refresh your page and look at the example again."
              ]
        , code: normalize
            """
            defaultTypewriter
              { words = repeat "Wisdom is the offspring of suffering and time."
              , initialMode = Deleting
              }
            """
        , typewriter: Just $ typewriterHtml input
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
              [ span
                  [ css "pr-2 has-text-weight-bold is-size-5", Aria.hidden "true" ]
                  [ text ">" ]
              , typewriter
              ]
            Nothing -> []

      , div
          [ css "card"
          , style $ do
              overflow overflowAuto
              maxHeight $ vh 40.0
          ]
          [ pre
              [ style $ textWhitespace whitespacePreWrap ]
              [ code [ css "language-haskell" ] [ text template.code ] ]
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
                          [ text "Is "
                          , typewriterHtml subtitle
                          ]
                      , quickstartExample
                      , singleWordExample
                      , multipleWordsExample
                      , finiteRunsExample
                      , typingSpeedExample
                      , jitterExample
                      , emptyCursorExample
                      , deletingExample
                      ]
                  ]
              ]
          ]
      ]
