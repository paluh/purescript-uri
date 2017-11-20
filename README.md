## "LTS" fork of purescript-uri

I use this fork in production. I also merge all fixes from upstream repo here.

This fork fixes all failing test cases from `purescript-uri` and also fixes some inconsistenices in uri representation against the spec. Additionaly it treats `+` as encoded space in query keys and values (which is not strictly compliant with RFC 3986 but is wide spread behavior - more on this topic here: https://github.com/slamdata/purescript-uri/issues/38).

The main implementation difference is that in this fork uri path is represented just by `String`. You can read more about my motivation here:

https://github.com/slamdata/purescript-uri/issues/15


# purescript-uri

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-uri.svg)](https://github.com/slamdata/purescript-uri/releases)
[![Build status](https://travis-ci.org/slamdata/purescript-uri.svg?branch=master)](https://travis-ci.org/slamdata/purescript-uri)
[![Dependency status](https://img.shields.io/librariesio/github/slamdata/purescript-uri.svg)](https://libraries.io/github/slamdata/purescript-uri)

A type-safe parser, printer, and ADT for URLs and URIs based on [RFC 3986](http://tools.ietf.org/html/rfc3986).

## Installation

```
bower install purescript-uri
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-uri).
