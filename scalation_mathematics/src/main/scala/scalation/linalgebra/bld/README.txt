
Code Generation:

The `bld` package contain source for generating code that can be copied into the
directory below.

It uses string interpolation '$variable' to substitute strings into variables.
The code to be generated is simply a long raw string (raw""" ... """) that
contains the code, including several variables ($variable).

Since Scala 2.12.5, very long raw strings are not well supported and
may cause the compiler to go into an infinite loop. 
A work-around is to break the code into multiple raw strings.
Raw string lengths of about 100 locs seem to work.

Also see `linalgebra.mem_mapped.bld` and `util.bld`.

