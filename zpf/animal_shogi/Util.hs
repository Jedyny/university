module Util where

import Control.Applicative
import Data.Maybe

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

{-- GUI utils --}

(?) :: Window -> String -> IO Element
window ? name = fromJust <$> window `getElementById` name

{-- Firefox requires camelCase; Chrome does not care --}
setBackground :: IO Element -> String -> IO Element
setBackground e url =
  e # set style [("backgroundImage", "url('" ++ url ++ "')")]


