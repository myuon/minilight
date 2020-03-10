# minilight

[![CircleCI](https://circleci.com/gh/myuon/minilight.svg?style=svg)](https://circleci.com/gh/myuon/minilight) [![Hackage](http://img.shields.io/hackage/v/minilight.svg)](https://hackage.haskell.org/package/minilight) [![MIT license](http://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A simple but powerful graphics library.

*NB: This library is fairly unstable and highly experimental.*

## Build

You first need to install [sdl2](https://www.libsdl.org/index.php) packages.

For Ubuntu 18.04:

```sh
~$ sudo apt install -y libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-gfx-dev
```

For ghc-8.8, use a version `>= 0.4.3`

## Tutorial

Create a new project. Assume that you have the `resource` directory under the project root, which contains the resource stuff.

Here is an example, specifying `resources/app.yml` for the configuration file and `resources` for the watching directory (dynamic hot reloading).

```hs
import Control.Monad
import Data.Component.Resolver (resolver)
import MiniLight

main :: IO ()
main = runLightT $ do
  runMiniloop
    ( defConfig { appConfigFile        = Just "resources/app.yml"
                , hotConfigReplacement = Just "resources"
                , componentResolver    = resolver
                }
    )
    ()
    return
```

In `resources/app.yml`, you can write your application configuration.

```yaml
_vars:
  window:
    width: 800
    height: 600
app:
  - name: layer
    properties:
      image: resources/background.png
      position:
        x: 0
        y: 0
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
      next:
        image: resources/text-pause.png
        division:
          x: 1
          y: 6
        interval: 40
      engine:
        messages: This is a message.
        color: [255,255,255,255]
        font:
          family: IPAGothic
          size: 22
```

For the configuration syntax, see [MiniLight.Loader](https://hackage.haskell.org/package/minilight/docs/MiniLight-Loader.html).

For the pre-defined components, see the modules under `Data.Component`.

## Examples

See [examples](https://github.com/myuon/minilight/tree/master/examples)
