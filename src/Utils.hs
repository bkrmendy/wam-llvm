module Utils where

import Data.Text

showT :: (Show a) => a -> Text
showT = pack . show
