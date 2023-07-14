module Css where

import Prelude

import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties (class_)

-- | Taken from real world halogen:
-- | https://github.com/thomashoneyman/purescript-halogen-realworld/blob/7acf8e30b4f32ba956a8162e7b33db1a353bf112/src/Component/HTML/Utils.purs#L11-L14C33
css :: ∀ r i. String -> IProp (class :: String | r) i
css = class_ <<< ClassName
