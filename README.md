# FLAC for Haskell

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/flac.svg?style=flat)](https://hackage.haskell.org/package/flac)
[![Stackage Nightly](http://stackage.org/package/flac/badge/nightly)](http://stackage.org/nightly/package/flac)
[![Stackage LTS](http://stackage.org/package/flac/badge/lts)](http://stackage.org/lts/package/flac)
![CI](https://github.com/mrkkrp/flac/workflows/CI/badge.svg?branch=master)

* [Aims of the project](#aims-of-the-project)
* [Motivation](#motivation)
* [Provided functionality](#provided-functionality)
* [Limitations](#limitations)
* [Quick start](#quick-start)
* [Related packages](#related-packages)
* [Contribution](#contribution)
* [License](#license)

This is a complete high-level Haskell binding to
[libFLAC](https://xiph.org/flac/)—reference FLAC implementation.

> As the maintainer of the C FLAC code base, I must say I'm impressed. Quite
> honestly, I think the C API is horrible.
>
> —[Erik de Castro Lopo](https://www.reddit.com/r/haskell/comments/5lyk70/announcing_flac_a_complete_highlevel_binding_to/dc00yb7/)

## Aims of the project

These are the goals of the project:

* Be a complete interface for FLAC file manipulation in Haskell.
* Be as efficient as the underlying C implementation.
* Provide a safe API using type system to kindly guard against bad things,
  but not too much so as to remain beginner-friendly and simple.

## Motivation

FLAC is awesome and Haskell is awesome, surely there should be a safe
Haskell API to the fast libFLAC library!

Seriously though, we have
[`htaglib`](https://hackage.haskell.org/package/htaglib) to work with audio
metadata, but it does not support FLAC-specific thing I would like to
manipulate. We have
[`hsndfile`](https://hackage.haskell.org/package/hsndfile), but I don't
really want to read FLAC data into a buffer or Haskell `Vector`. How simple
is it (if possible) to decode a FLAC file using that library? How simple is
it to figure out where to begin with such a task? With `flac` it is one line
of code.

## Provided functionality

`flac` can work with:

* Metadata—full support for reading/writing/deleting of all audio
  parameters, application data, seek tables, vorbis comments of all sorts,
  CUE sheets, and even pictures.

* Stream decoder—simple interface for decoding to WAVE and RF64.

* Stream encoder—a lot of options to tweak, everything that libFLAC
  supports.

## Limitations

Right now there are three main limitations:

* No Ogg FLAC support, and I do not plan to add it; I'll accept a PR adding
  support for Ogg FLAC.

* It's not possible to use custom callbacks for printing decoding/encoding
  progress in real-time.

* Only works on little-endian architectures so far; I'll accept a PR lifting
  this limitation.

## Quick start

The best way to start using `flac` is to take a look at [the
Haddocks](https://hackage.haskell.org/package/flac). Encoding and decoding
should be simple to understand, for metadata there are examples in the docs.
Feel free to ask me a question if you get stuck with something.

## Related packages

The following packages are designed to be used with `flac`:

* [`flac-picture`](https://hackage.haskell.org/package/flac-picture)—add
  pictures to FLAC metadata easier.

## Contribution

Please direct all issues, bugs, and questions to [the GitHub issue tracker
for this project](https://github.com/mrkkrp/flac/issues).

Pull requests are also welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.
