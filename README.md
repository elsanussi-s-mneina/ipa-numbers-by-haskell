# IPA character encoded in Unicode, and as IPA Numbers conversion

This project converts IPA numbers (such as 123) to IPA characters (such as ʀ, which is the symbol for the voiced uvular trill, encoded in unicode at U+0280).

This is a compiled, command line program.

It was originally part of the https://github.com/elsanussi-s-mneina/phonetics-modeling project.

## Important References:
These are the data that this program is trying to implement:

The IPA Number chart:
https://www.internationalphoneticassociation.org/sites/default/files/IPA_Number_chart_(C)2005.pdf

# Project status
- This part is incomplete, come back in a week it will probably be complete by then.
- currently conversion is both ways, which direction can be specified using command line flags.
- unit tests have been migrated over from the parent project.

## Tasks left to do:
- go over unit tests and ensure all cases are covered.


## Developer software requirements
- Glasgow Haskell Compiler

## Developer knowledge requirements
- how to type standard input onto console, (hint use ctrl-D on a blank line to end the file) or how to redirect input from a file to a program.


## Basics of how to use:
### How to compile
Either run the file titled `compile.sh` or run the commands within it.

`./compile.sh`

If it does not work, run clean.sh first.

### How to run
`/.run.sh`

Then you have to provide input, something like "123" without the quotes.
Then enter a new line, and then hold the ctrl key and press the D key.

### How to run unit tests
`test.sh`
