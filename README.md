# posix-regex

[CHICKEN][chicken web] egg for POSIX regular expressions supporting [POSIX EREs and BREs][opengroup regex].

## Motivation

[CHICKEN's default regex library][chicken irregex] only supports POSIX EREs with PCRE extensions.
Unfourtunatly, I needed a strict POSIX-compliant BRE implementation, without any extensions, for [my implementation of the ed text editor][edward github] in CHICKEN Scheme.
For this reason, I originally used CHICKEN's Foreign Function Interface to use the POSIX `regcomp(3)` and `regexec(3)` functions directly in the codebase of my editor.
However, the code for wrapping these functions became more complex over time (e.g. due to support for better error messages via `regerror(3)`).
As such, I decided to move it to a dedicated library which may be useful for other CHICKEN Schemers which are looking for a pure POSIX BRE or ERE implementation.

## Features

* Thin wrappers for `regcomp(3)` and `regexec(3)`
* Error handling via [R7RS][r7rs] exceptions and a `regerror(3)` wrapper
* API with strict [CHICKEN][chicken types] type annotations
* Support for submatches via R7RS bytevectors

## Installation

This library is available in the CHICKEN egg repository and can be installed using:

	$ chicken-install posix-regex

## Documentation

The documentation is maintained separately in the [CHICKEN wiki][chicken posix-regex].
Initially, the documentation was partially generated with [schematic][chicken schematic] from inline comments using `schematic-wiki -c ';;>' < posix-regex.scm`.

## Usage

For an elaborate usage example, refer to the source code of the [edward][edward github] text editor.

## License

This library is licensed under [GPL-3.0-only][spdx gpl-3.0-only].

[chicken web]: https://call-cc.org
[opengroup regex]: https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html
[chicken irregex]: https://wiki.call-cc.org/man/5/Module%20(chicken%20irregex)
[edward github]: https://github.com/nmeum/edward
[r7rs]: https://small.r7rs.org/
[chicken types]: https://wiki.call-cc.org/man/5/Types
[chicken posix-regex]: https://wiki.call-cc.org/eggref/5/posix-regex
[spdx gpl-3.0-only]: https://spdx.org/licenses/GPL-3.0-only.html
[chicken schematic]: https://wiki.call-cc.org/eggref/5/schematic
