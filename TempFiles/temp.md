

Show TA 

• The definition of the EDSL (FP3.1),
• The set of mandatory features,




PComb.hs -- most general thing
	* a parser parse a stream into [(a, stream)]; 
	  instance of Functor, Applicative, Alternative
	* a func `char :: Char -> Parser Char`


BasicParsers.hs -- parser that can parse small chunk of stuff
	* analogous to these things in parsec
		```
		identifier = Token.identifier lexer
		integer    = Token.integer lexer
		parens     = Token.parens lexer
		symbol     = Token.symbol lexer
		reserved   = Token.reserved lexer
		```

MicroFP.hs -- most top-level thing
	* EDSL definition; sample code in EDSL
	* parser that can parse larger chunk, ie factor, term, expr



















