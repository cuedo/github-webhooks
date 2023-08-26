![github-webhooks Mascot](./doc/github-webhooks-mascot-248.svg) \
github-webhooks
---------------

![GitHub release (latest by date)](https://img.shields.io/github/v/release/cuedo/github-webhooks)
[![Hackage](https://img.shields.io/hackage/v/github-webhooks.svg)](https://hackage.haskell.org/package/github-webhooks)
[![License](https://img.shields.io/github/license/cuedo/github-webhooks.svg)](#license)

Complete Haskell types and instances for decoding GitHub API [Webhook] payloads.

### Build Status

|             | `release` | `develop` |
|-------------|---------|----------|
| Lints  | [![scan-code](https://github.com/cuedo/github-webhooks/actions/workflows/scan-code.yml/badge.svg?branch=release)](https://github.com/cuedo/github-webhooks/actions/workflows/scan-code.yml) | [![scan-code](https://github.com/cuedo/github-webhooks/actions/workflows/scan-code.yml/badge.svg?branch=develop)](https://github.com/cuedo/github-webhooks/actions/workflows/scan-code.yml) |
| Ubuntu | [![test-ubuntu-latest](https://github.com/cuedo/github-webhooks/actions/workflows/test-ubuntu-latest.yml/badge.svg?branch=release)](https://github.com/cuedo/github-webhooks/actions/workflows/test-ubuntu-latest.yml) | [![test-ubuntu-latest](https://github.com/cuedo/github-webhooks/actions/workflows/test-ubuntu-latest.yml/badge.svg?branch=develop)](https://github.com/cuedo/github-webhooks/actions/workflows/test-ubuntu-latest.yml) |
| Mac OS | [![test-macos-latest](https://github.com/cuedo/github-webhooks/actions/workflows/test-macos-latest.yml/badge.svg?branch=release)](https://github.com/cuedo/github-webhooks/actions/workflows/test-macos-latest.yml) | [![test-macos-latest](https://github.com/cuedo/github-webhooks/actions/workflows/test-macos-latest.yml/badge.svg?branch=develop)](https://github.com/cuedo/github-webhooks/actions/workflows/test-macos-latest.yml) |
| Windows | [![test-windows-latest](https://github.com/cuedo/github-webhooks/actions/workflows/test-windows-latest.yml/badge.svg?branch=release)](https://github.com/cuedo/github-webhooks/actions/workflows/test-windows-latest.yml) | [![test-windows-latest](https://github.com/cuedo/github-webhooks/actions/workflows/test-windows-latest.yml/badge.svg?branch=develop)](https://github.com/cuedo/github-webhooks/actions/workflows/test-windows-latest.yml) |

### Table of Contents

* [Features](#features)
* [Installation](#installation)
* [Examples](#examples)
* [Changelog](#changelog)
* [Roadmap](#roadmap)
* [Authors](#authors)
* [Support](#support)
* [License](#license)

## Features
`github-webhooks` comes with all the bells and whistles:

* **Best-in-class JSON Decoding via `Aeson`**

  This library uses the [aeson] package, a fast and widely adopted Haskell library for working with JSON data. It ensures best-in-class JSON decoding, enabling you to parse GitHub webhooks payloads with high performance and accuracy.

* **i18n/Unicode String Support via `Text`**

  `github-webhooks` has first-class support for Unicode strings in all functions via the [text] package. This ensures that the library can handle any i18n text data in the payloads correctly, regardless of the character set.

* **Suitable for Large Data Payloads using `Vector`**

  This library uses the [vector] package to handle large data payloads efficiently. This means it is suitable for big data workloads and capable of processing large amounts of data without excessive CPU or memory usage.

* **Type-safe Encoding of Optional Data**

  Optional data in the payloads are encoded in a type-safe manner (i.e. using `Maybe`). This ensures that your application will not encounter runtime errors due to missing or unexpected data and provides valuable signals during development where you should implement fault handling.

* **Type-safe Event Action Encodings with Support for Future API Changes**

  This library encodes event actions in a type-safe manner that does not require the payload to be fully decoded. Thus, applications built using this library will generally be forward-compatible with future changes to the GitHub API and continue to work even if GitHub makes changes to the webhooks API.

* **Instances for `Typeable`, `Data`, `Generic` and `NFData`**

  The data types in this library have instances for `Typeable`, `Data`, `Generic`, and `NFData`. This ensures compatibility with a wide range of Haskell libraries and metaprogramming tools.

* **Strongly Typed Utility Classes `EventHasSender`, `EventHasRepo` et al.**

  This library provides strongly typed utility classes such as `EventHasSender` and `EventHasRepo` to make it easier to work with the different event types and their associated data. You can define webhook event handlers that work on multiple events.

* **Strict Data Types Suitable for High Performance Streaming Operations**

  The data types in this library are strict, making them suitable for high-performance streaming operations. This ensures that your application can process large streams of webhook events efficiently and without space leaks.

* **Full Support for Webhook Event Types**

  `github-webhooks` provides full support for [all the GitHub webhook event types](https://developer.github.com/v3/activity/events/types/#event-types--payloads). This means you can handle any event sent by GitHub without having to implement custom parsing or processing logic.

## Installation
`github-webhooks` is available via installation on [Hackage](https://hackage.haskell.org/package/github-webhooks) and [Stackage](https://www.stackage.org/package/github-webhooks).

## Examples

### Minimal interactive example:

Shell:
```hs
$ stack ghci bytestring aeson github-webhooks
import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson           ( eitherDecode' )

json <- BSL.readFile "fixtures/watch-event.json"
eitherDecode' json :: Either String WatchEvent
```

Output:
```
Right (WatchEvent {
  evWatchAction = WatchStartedAction,
  evWatchRepo = HookRepository {
    whRepoId = 35129377,
    whRepoNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg=",
    whRepoName = "public-repo",
    whRepoFullName = "baxterthehacker/public-repo",
    whRepoOwner = Right (HookUser {
      whUserLogin = "baxterthehacker",
      whUserId = 6752317,
      whUserNodeId = "MDg6Q2hlY2tSdW4xMjg2MjAyMjg=",
      whUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/6752317?v=3",
...
```
[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/cuedo/github-webhooks)

### Getting started quickly
There are some turnkey integration tutorials in this repository:

* **[Servant Integration (Simple)](./examples/servant-simple#readme)**

  Minimal working example of integrating this package with [servant] and [servant-github-webhook].

* **[Servant Integration (Typical)](./examples/servant#readme)**

  This example demonstrates more advanced use-cases with [servant] by decoding multiple different types of payloads on the same endpoint.

* **[Scotty Integration](./examples/scotty#readme)**

  Minimal working example of integrating this package with [scotty].

## Changelog
See [CHANGELOG.md](./CHANGELOG.md) for a summary of changes in each release.

## Roadmap
See [ROADMAP.md](./ROADMAP.md) for the project timeline and feature estimation of future releases.

## Authors
See [AUTHORS](./AUTHORS) for a list of significant authors.

## Support
The best way to get free community support is to raise a well-written issue in this repository and one of the maintainers will endeavour to respond to it quickly.

If you are using a plugin i.e. [servant-github-webhook](https://github.com/tsani/servant-github-webhook), you might be able to get better support if you raise your issue against that project instead.

For enterprise support, you can write us: [foss@cuedo.com.au](mailto:foss@cuedo.com.au).

## License
`github-webhooks` is free open source software, however you must use it according to the MIT-style [LICENSE](./LICENSE).

    MIT License

    Copyright (c) 2017-2023 CUEDO CONTROLS P/L (https://cuedo.com.au)

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

Free open source software, sponsored by CUEDO CONTROLS P/L ([cuedo.com.au](https://cuedo.com.au)). \
\
<a href="https://cuedo.com.au" rel="sponsor">![CUEDO CONTROLS PTY LTD](./doc/cuedo-color-256.svg)</a>

[Webhook]: https://developer.github.com/webhooks/

[aeson]: https://www.stackage.org/package/aeson
[text]: https://www.stackage.org/package/text
[vector]: https://www.stackage.org/package/vector
[servant]: https://www.stackage.org/package/github-webhooks
[servant-github-webhook]: https://www.stackage.org/package/servant-github-webhook
[scotty]: https://www.stackage.org/package/scotty
