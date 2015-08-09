# xRadar [![Build Status](http://img.shields.io/travis/dhleong/x-radar.svg?style=flat)](https://travis-ci.org/dhleong/x-radar)

A cross-platform, open source radar client for the 
[Vatsim](http://www.vatsim.net) network.

## Features

In addition to being cross-platform,
xRadar is intended to be highly customizable and is inspired
somewhat by [http://www.vim.org](Vim). It is designed with the 
philosophy that everything should be doable from the keyboard,
and preferably no more than a couple strokes away.

For example, the current method of selecting aircraft is by
pressing `s` followed by two letters (that appear next to
the aircraft on the radar). Once selected, its flight plan
may be opened and edited using the key sequence `op`.

xRadar supports the notion of "modes," with different key
strokes available in different modes. Not much is done with
this yet, but the support is there.

xRadar intends to be cross-compatible with existing radar
files. It currently reads (most of) the `.sct2` file format 
for displaying scenery.

## Installation

xRadar is still under development, but you can play with it
by cloning the repo and using [leiningen](http://leiningen.org/).
A function called `testing` is provided in the `xradar.radar`
namespace that builds a radar view and ads some test aircraft,
but you'll want to tweak it. It assumes a `ZNY.sct2` file in
a `~/VRC/Support` directory.

## License

Copyright © 2015 Daniel Leong

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
