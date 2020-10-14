![github-webhooks Mascot](./doc/github-webhooks-mascot-248.svg) \
github-webhooks
---------------

[![Hackage](https://img.shields.io/hackage/v/github-webhooks.svg?style=flat-square)](https://hackage.haskell.org/package/github-webhooks)
[![License](https://img.shields.io/github/license/onrock-eng/github-webhooks.svg?style=flat-square)](#license)

|             | `release` | `master` |
|-------------|---------|----------|
| Linux & OSX | [![travis-linux](https://img.shields.io/travis/onrock-eng/github-webhooks/release.svg?style=flat-square)](https://travis-ci.org/onrock-eng/github-webhooks) | [![travis-linux](https://img.shields.io/travis/onrock-eng/github-webhooks.svg?style=flat-square)](https://travis-ci.org/onrock-eng/github-webhooks) |
| Windows     | [![appveyor-windows](https://img.shields.io/appveyor/ci/OnRockEngineering/github-webhooks/release.svg?style=flat-square)](https://ci.appveyor.com/project/OnRockEngineering/github-webhooks) | [![appveyor-windows](https://img.shields.io/appveyor/ci/OnRockEngineering/github-webhooks/master.svg?style=flat-square)](https://ci.appveyor.com/project/OnRockEngineering/github-webhooks) |

Complete Haskell types and instances for decoding GitHub API [Webhook] payloads.

* [Features](#features)
* [Installation](#installation)
* [Examples](#examples)
* [Changelog](#changelog)
* [Roadmap](#roadmap)
* [Authors](#authors)
* [License](#license)

## Features
* Best-in-class JSON decoding via [aeson].
* Unicode string support via [text].
* Suitable for large data payloads using [vector].
* Type-safe encoding of optional data.
* Type-safe event action encodings with support for future API changes.
* Instances for Typeable, Data, Generic and NFData.
* Strongly typed utility classes EventHasSender, EventHasRepo et al.
* Strict data types suitable for high performance streaming operations.
* Full support for [these event types](https://developer.github.com/v3/activity/events/types/#event-types--payloads).

## Installation
`github-webhooks` is available via installation on [Hackage](https://hackage.haskell.org/package/github-webhooks) and [Stackage](https://www.stackage.org/package/github-webhooks).

## Examples
Minimal working example:
```hs
$ stack ghci bytestring aeson github-webhooks
import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson           ( eitherDecode' )

json <- BSL.readFile "fixtures/watch-event.json"
eitherDecode' json :: Either String WatchEvent
```

Some practical integration examples are also provided:
* [Servant Integration (Simple)](./examples/servant-simple#readme)  
  Minimal working example of integrating this package with [servant] and [servant-github-webhook].
* [Servant Integration (Typical)](./examples/servant-adv#readme)  
  This example demonstrates more advanced use-cases with [servant] by decoding multiple different types of payloads on the same endpoint.
* [Scotty Integration](./examples/scotty#readme)  
  Minimal working example of integrating this package with [scotty].

## Changelog
See [CHANGELOG.md](./CHANGELOG.md) for a summary of changes in each release.

## Roadmap
See [ROADMAP.md](./ROADMAP.md) for the project timeline and feature estimation of future releases.

## Authors
See [AUTHORS](./AUTHORS) for a list of significant authors.

## License

    MIT License

    Copyright (c) 2017-2020 Cuedo Business Solutions (https://cuedo.com.au)

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

Free open source software, sponsored by [cuedo.com.au](https://cuedo.com.au). \
\
<a href="https://cuedo.com.au" rel="sponsor">![Cuedo Business Solutions](./doc/cuedo-color-256.svg)</a>

[Webhook]: https://developer.github.com/webhooks/

[aeson]: https://www.stackage.org/package/aeson
[text]: https://www.stackage.org/package/text
[vector]: https://www.stackage.org/package/vector
[servant]: https://www.stackage.org/package/github-webhooks
[servant-github-webhook]: https://www.stackage.org/package/servant-github-webhook
[scotty]: https://www.stackage.org/package/scotty
