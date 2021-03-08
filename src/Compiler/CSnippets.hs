{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Compiler.CSnippets where

import Data.Text as T
import NeatInterpolation (text)

register :: Text -> Text -> Text
register r n = [text|${r}(${n})|]

functor :: Text -> Text -> Text
functor name arity = [text|Structure f = { .name = ${name}, .arity = ${arity} };|]

putStructure :: Text -> Text -> Text
putStructure f r = [text| {
                             $f
                             put_structure(f, ${r});
                          }
                        |]

setVariable :: Text -> Text
setVariable r = [text|set_variable(${r});|]

setValue :: Text -> Text
setValue r = [text|set_value(${r});|]

getStructure :: Text -> Text -> Text
getStructure f r = [text| {
                             $f
                             get_structure(f, ${r});
                          }
                        |]

unifyVariable :: Text -> Text
unifyVariable r = [text|unify_variable(${r});|]

unifyValue :: Text -> Text
unifyValue r = [text|unify_value(${r});|]

call :: Text -> Text
call f = [text|${f}();|]

proceed :: Text
proceed = T.empty

allocate :: Text -> Text
allocate n = [text|allocate($n);|]

deallocate :: Text
deallocate = [text|deallocate();|]

notImplemented :: Text -> Text
notImplemented s = [text| // Not implemented: ${s}|]