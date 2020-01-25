The idea is to pipe a stream of strings into a stream of collection of cells.
Using streams makes it possible to deal with infinitely large or even infinite input.
Naturally CSV file rows are delimited by EOLs, additionally w standard Scala library provides IO facilities to read lines 
providing  natural buffering.
from files. Line, however, may be 'carried over' if unclosed quotation is 
found.
RegExps are not used deliberately.  
For 'headless' stream, no assumptions are made about the structure (i.e. the number of cells in a row)
Headed CVS stream, that is just a pipe for scanner stream, would match the number of cells in a
row according to the header adding missing and throwing away exceeds.
