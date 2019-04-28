{-| The package provides the configuration loader.

An configuration example:

@
_vars:
  window:
    width: 800
    height: 600
app:
  - name: message-layer
    properties:
      window:
        image: resources/window-base.png
        position:
          x: 0
          y: ${${var:window.height} - ${ref:..size.height}}
        size:
          width: ${var:window.width}
          height: 150
@

== Syntax

=== @_vars@

You can define a new variable. Use object syntax under the @_vars@ field.

The variables can be referenced in all siblings and under their siblings to the @_vars@, in the variable syntax @${var:_path_}@.

=== Expr

In each field, you can specify an expression defined in the loader.

- @${}@: enclose the expr by @${}@, to tell the parsers that the field is an expr not a plain string.
- @${ref:_path_}@: specify any path to refer any other value. The path resolution is performed once, not recursively resolved. @_path_@ consists of field names splitted by a period. Use double dots @..@ for a parent.
- @${var:_path_}@: specify any path to value defined at the field. @_path_@ consists of field names splitted by a period.
- arithmetic operator: addition, subtraction, multiplication and division (@+,-,*,/@) can also be used in @${}@.

-}
module MiniLight.Component (
  module MiniLight.Component.Types,
  module MiniLight.Component.Loader,

  Resolver,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import MiniLight.Light
import MiniLight.Component.Types
import MiniLight.Component.Loader

type Resolver = T.Text -> Aeson.Value -> MiniLight Component

