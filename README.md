# MoCaml - A mocking framework for OCaml

The goal of this library is to provide a framework for automatically generating mock modules for OCaml. This library is inspired by the excellent JMock library for Java, and follows the patterns of the also excellent xUnit book.

This is currently a work in progress, and right now is little more than a scratch pad for working out implementation ideas. Hopefully there will be something more usable soon.

## Building

This library will be written for OCaml 4.0 (GADTs and new syntax for first class modules). We will also use ocp-build to build the library. The easiest way to get started with building this library is to use opam:

```
# Set up opam
git clone git://github.com/OCamlPro/opam.git
cd opam ; ./configure ; make ; sudo make install
opam init ; opam update
# Install and set up OCaml 4.0
opam switch 4.00.0
eval `opam config -env`
# Install ocp-build
opam install ocp-build
# Build a test program in this repo
cd <path/to/mocaml>
ocp-build fcm-test
```
