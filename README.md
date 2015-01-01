# Mercury-JSON

'mercury_json' is a Mercury library for reading and writing
[JSON](http://www.json.org) from character streams.


## FEATURES

* conforms to RFC 7159
* optionally allows comments in JSON
* optionally allows trailing commas in JSON objects and arrays
* optionally allows numbers to be -Infinity / Infinity
* user-configurable behaviour for handling repeated members in objects
* marshaling and unmarshaling of Mercury data to and from JSON


## LICENSE

'mercury_json' is licensed under a simple 2-clause BSD style license.  See the
file [COPYING](COPYING) for details.


## INSTALLATION

Check the values in the file [Make.options](Make.options) to see if they agree
with your system, then do:

    $ make install


You can also override values in [Make.options](Make.options) on the command
line, for example

    $ make INSTALL_PREFIX=/foo/bar install

causes the library to be installed in the directory **/foo/bar**.


## TESTING

To run the regression test suite, do:

    $ make runtests

## EXAMPLES

The [samples](samples) directory contains some example programs that
demonstrate different features of the library.
