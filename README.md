# Mercury-JSON

`mercury_json` is a [Mercury](http://www.mercurylang.org) library for reading
and writing [JSON](http://www.json.org) from character streams.

## Features

* conforms to [RFC 8259](https://www.rfc-editor.org/rfc/rfc8259.txt)
* optionally allows single and multiline comments in JSON
* optionally allows single trailing commas in JSON objects and arrays
* optionally allows numbers to be -Infinity / Infinity
* user configurable behaviour for handling repeated object members
* marshaling and unmarshaling of Mercury data to and from JSON
* user configurable maximum nesting depth limit

## License

`mercury_json` is licensed under a simple 2-clause BSD style license.  See the
file [COPYING](COPYING) for details.

## Installation

Check the values in the file [Make.options](Make.options) to see if they agree
with your system, then do:

    $ make install

You can also override values in [Make.options](Make.options) on the command
line, for example

    $ make INSTALL_PREFIX=/foo/bar install

causes the library to be installed in the directory `/foo/bar`.

## Testing

To run the regression test suite, do:

    $ make runtests

## Examples

The [samples](samples) directory contains some example programs that
demonstrate different features of the library.

## Mercury 20.06.X Compatibility

The code on the master branch is **not** compatible with Mercury 20.06.X.
If you require a version of `mercury_json` that works with Mercury 20.06.X,
then checkout the `mercury_20_06` branch.

## Mercury 20.01.X Compatibility

The code on the master branch is **not** compatible with Mercury 20.01.X.
If you require a version of `mercury_json` that works with Mercury 20.01.X,
then checkout the `mercury_20_01` branch.

## Mercury 14.01.X Compatibility

The code on the master branch is **not** compatible with Mercury 14.01.X.
If you require a version of `mercury_json` that works with Mercury 14.01.X,
then checkout the `mercury_14_01` branch.

## Author

Julien Fischer <juliensf@gmail.com>
