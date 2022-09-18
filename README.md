# libjuice-hs

Haskell bindings to [libjuice - UDP Interactive Connectivity Establishment](https://github.com/paullouisageneau/libjuice/issues/79)

libjuice (JUICE is a UDP Interactive Connectivity Establishment library) allows to open bidirectionnal User Datagram Protocol (UDP) streams with Network Address Translator (NAT) traversal.

# License

Note that while all code in this repository is licensed unter der MIT license,
libjuice itself is licensed under LGPL v2.1, so the combined project is also
licensed under LGPL v2.1


## How to build:

* Run `make` to build the project
* Run `make test` to run the test suite

## Requirements:
* stack
* make

# How to update the c source code:
* Requires: git-subrepo
* run:
```
git subrepo fetch libjuice
