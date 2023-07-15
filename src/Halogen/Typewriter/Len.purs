-- | Unfortunately PureScript does not have a template-haskell equivalent.
-- | This module manually defines lenses for typewriter's input and state.
module Halogen.Typewriter.Lens where

import Data.Lens.Record (prop)
import Type.Prelude (Proxy(..))

words = prop (Proxy :: Proxy "words")
outputText = prop (Proxy :: Proxy "outputText")
typeDelay = prop (Proxy :: Proxy "typeDelay")
deleteDelay = prop (Proxy :: Proxy "deleteDelay")
pauseDelay = prop (Proxy :: Proxy "pauseDelay")
cursorDelay = prop (Proxy :: Proxy "cursorDelay")
mode = prop (Proxy :: Proxy "mode")
cursor = prop (Proxy :: Proxy "cursor")
cursorHidden = prop (Proxy :: Proxy "cursorHidden")
jitter = prop (Proxy :: Proxy "jitter")
running = prop (Proxy :: Proxy "running")
