![github-webhooks Mascot](./doc/github-webhooks-mascot-248.svg)<br/>
github-webhooks
---------------

[![Hackage](https://img.shields.io/hackage/v/github-webhooks.svg?style=flat-square)](https://hackage.haskell.org/package/github-webhooks)
[![Stackage](http://stackage.org/package/github-webhooks/badge/nightly?style=flat-square)](https://www.stackage.org/package/github-webhooks)
[![License](https://img.shields.io/github/license/onrock-eng/github-webhooks.svg?style=flat-square)](#license)

|             | `lts-10.0` |
|-------------|------------|
| Linux & OSX | [![Linux & OSX](https://img.shields.io/travis/onrock-eng/github-webhooks/master.svg?style=flat-square)]() |
| Windows     | [![Windows](https://img.shields.io/appveyor/ci/OnRockEngineering/github-webhooks/master.svg?style=flat-square)]() |

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
* [Servant Integration (Simple)](./examples/servant-simple#readme)<br/>
  Minimal working example of integrating this package with [servant] and [servant-github-webhook].
* [Servant Integration (Typical)](./examples/servant-adv#readme)<br/>
  This example demonstrates more advanced use-cases with [servant] by decoding multiple different types of payloads on the same endpoint.
* [Scotty Integration](./examples/scotty#readme)<br/>
  Minimal working example of integrating this package with [scotty].

## Changelog
See [CHANGELOG.md](./CHANGELOG.md) for a summary of changes in each release.

## Roadmap
See [ROADMAP.md](./ROADMAP.md) for the project timeline and feature estimation of future releases.

## Authors
See [AUTHORS](./AUTHORS) for a list of significant authors.

## License
See [LICENSE](./LICENSE) for a bundled copy of the MIT license.

<br/>

> ###### Free open source software, sponsored by
> <a href="https://onrock.engineering" rel="sponsor">![OnRock Engineering](./doc/onrock-color-196.svg)</a>

[Webhook]: https://developer.github.com/webhooks/

[aeson]: https://www.stackage.org/package/aeson
[text]: https://www.stackage.org/package/text
[vector]: https://www.stackage.org/package/vector
[servant]: https://www.stackage.org/package/github-webhooks
[servant-github-webhook]: https://www.stackage.org/package/servant-github-webhook
[scotty]: https://www.stackage.org/package/scotty
