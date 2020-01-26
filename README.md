####General idea and notes on implementation
The idea is to pipe a stream of strings into a stream of collection of cells. Using streams makes it possible to deal 
with infinitely large or even infinite input. 
This is a hybrid solution that combines elements of both FP and OO. For example, in a pure FP solution the hierarchy of
_ParserEvent_ could be replaced with type classes.
The use of functional states in the test framework is disputable, however I decided to leave it as it is to demonstrate 
some techniques. Although _checkEvent_ call is referentially transparent FP purists would frown as it throws exception.
I find it acceptable in a test code, although in production I would rather lift the state into _Either_ or _MonadError_ 
 
####Scanning
RegExps are not used deliberately.
The parser generates a stream of "events" as it scans along the line. In case the line contains opened quotation (and 
therefore cannot produce a compete row) the scanner would pull another line from the input stream.   

####Output format
As it is not possible to figure out the number of columns until the input stream is exhausted the default output format
is just a stream of a collection of strings.
  
Headed CVS stream, that is just a pipe for scanner stream, would match the number of cells in a row according to the 
header adding missing and throwing away exceeds.

####Note on configurability, including EOL requirement
Naturally CSV file rows are delimited by EOLs, additionally standard Scala library provides IO facilities to read lines 
from files providing natural buffering.
Underlying implementation uses standard Java buffered reader that has limited support for
EOL configuration, in particular it is possible to 'omit LF'. In real life should this requirement occur I would try to 
clarify whether an arbitrary symbol can be used as EOL or the customer only cares about CR-LF-CRLF convention that is 
supported by the standard library out of the box.   

Other configurable elements are passed as implicit parameter, defaults may be imported as _implicits.defaultDelimiters_


####Command line demo
*MagicParserApp* takes the only parameter - path to a file. 

#####Encoding
There is a requirement that specifies that encoding other than UTF-8 should be supported. Out of the box 
_Source.fromFile_ call supports standard encoding as well as custom codecs. 

Although no configuration options is available for the app it may be easily added whether via parsing command line or 
by reading some configuration file and setting up an implicit instance of _Delimiters_ type.    