# FLAC for Haskell

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/flac.svg?style=flat)](https://hackage.haskell.org/package/flac)
[![Stackage Nightly](http://stackage.org/package/flac/badge/nightly)](http://stackage.org/nightly/package/flac)
[![Stackage LTS](http://stackage.org/package/flac/badge/lts)](http://stackage.org/lts/package/flac)
[![Build Status](https://travis-ci.org/mrkkrp/flac.svg?branch=master)](https://travis-ci.org/mrkkrp/flac)

* [Aims of the project](#aims-of-the-project)
* [Motivation](#motivation)
* [Provided functionality](#provided-functionality)
* [Limitations](#limitations)
* [Quick start](#quick-start)
* [Related packages](#related-packages)
* [Contribution](#contribution)
* [License](#license)

This is a complete high-level Haskell binding
to [libFLAC](https://xiph.org/flac/)—reference FLAC implementation.

> As the maintainer of the C FLAC code base, I must say I'm impressed. Quite
> honestly, I think the C API is horrible.
>
> —[Erik de Castro Lopo](https://www.reddit.com/r/haskell/comments/5lyk70/announcing_flac_a_complete_highlevel_binding_to/dc00yb7/)

## Aims of the project

Here are several ideas the project follows:

* Concentrate only on native FLAC format without messing with other audio
  formats or their flavors. This library is about FLAC only.

* Be a complete interface for FLAC file manipulation in Haskell. “Complete”
  means that everything supported by the reference implementation should be
  supported by this package. This is in the hope to prevent fragmentation
  and proliferation of different libraries to work with FLAC with each of
  them covering only some 80% of functionality the library author needed and
  neglecting other 20%.

* Be as efficient as the underlying C implementation, avoid adding any sort
  of overhead (memory/speed/or otherwise).

* Provide *idiot-proof* API using type system to kindly guard against bad
  things, but not so much to remain beginner-friendly and simple.

* Make invalid code and data unrepresentable.

## Motivation

FLAC is awesome and Haskell is awesome, surely there should be a way to
achieve an even higher level of awesomeness by coding a safe Haskell API to
the fast libFLAC library.

Seriously though, we
have [`htaglib`](https://hackage.haskell.org/package/htaglib) to work with
audio metadata, but it does not support a lot of FLAC-specific things I'd
love to manipulate. We
have [`hsndfile`](https://hackage.haskell.org/package/hsndfile), but I don't
really want to read FLAC data into a buffer or Haskell `Vector`. How simple
is it (if possible) to decode a FLAC file using that library? How simple is
it to figure out where to begin with such a task? With `flac` it's
`decodeFlac def "myfile.flac" "myfile.wav"`—done, average song in a fraction
of second.

## Provided functionality

Here we go:

* Metadata—full support for reading/writing/deleting of all audio
  parameters, application data, seek tables, vorbis comments of all sorts,
  CUE sheets, and even pictures.

* Stream decoder—simple interface for decoding to vanilla WAVE and RF64
  (support for files larger than 4 Gb).

* Stream encoder—a lot of options to tweak, everything that libFLAC has, but
  you can also use `def` and just encode vanilla WAVE or RF64 file into
  native FLAC. Simple and efficient.

## Limitations

Right now there are three main limitations:

* No Ogg FLAC support, and I do not plan to add it, but I'll accept a PR
  adding support for Ogg FLAC.

* It's not possible to use custom callbacks for printing decoding/encoding
  progress in real-time and stuff like that. Not a big issue IMO, given that
  we get nice and safe API instead.

* Only works on little-endian architectures so far, I'll accept a PR lifting
  this limitation.

## Quick start

The best way to start using `flac` is to take a look
at [the Haddocks](https://hackage.haskell.org/package/flac). Encoding and
decoding are so simple that even a baby could handle it, and for metadata
there are examples and a lot of details in the docs. Feel free to ask me a
question if you get stuck with something though.

## Related packages

The following packages are designed to be used with `flac`:

* [`flac-picture`](https://hackage.haskell.org/package/flac-picture)—add
  pictures to FLAC metadata easier.

## Contribution

Please kindly direct all issues, bugs, and questions to
[the GitHub issue tracker for this project](https://github.com/mrkkrp/flac/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2016–2019 Mark Karpov

Distributed under BSD 3 clause license.
