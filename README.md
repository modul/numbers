# Numbers

Numbers is a Haskell library and commandline tool to retrieve trivia about numbers from the open [Numbers API](numbersapi.com).

## Installation

Use [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) to install:

```sh
stack build
stack install
```

## Commandline usage

```sh
numbers [OPTIONS] [NUMBER | DATE]

Categories:
  -t --trivia                           Get a trivia entry (default)
  -m --math                             Get a math entry
  -y --year                             Get a year entry
  -d --date                             Get a date entry
Response options:
  -f --fragment                         Return the entry as a sentence
                                        fragment
     --default=MESSAGE                  Show this message if requested number
                                        has no entry
Lookup options:
  -n --notfound=CEIL | FLOOR | DEFAULT  Selects an alternative if requested
                                        number has no entry
     --min=INT                          Lower limit for random entries
     --max=INT                          Upper limit for random entries
Common flags:
  -h --help                             Display help message
  -V --version                          Print version information

NUMBER can be a single integer, an interval (from..to) or a
comma-separated list of both.

DATE specifies month and day of month separated by a dot, slash or dash.

Without any arguments a number or date is randomly chosen by the API.
```

## Library usage

All library modules should be documented well enough. You can build the docs using 
stack:

```sh
stack haddock
```