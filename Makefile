build : 
	stack build

clean :
	rm -f *.css

test :
	stack test

repl:
	stack ghci
