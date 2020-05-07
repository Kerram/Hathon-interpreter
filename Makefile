BNFC = CHANGE_ME # Type your localization of bnfc (probably in some .cabal/bin/bnfc)

all: interpreter

interpreter:
		cd src; $(BNFC) --haskell -p Syntax -m --functor Syntax/syntax.cf; make; ghc -o ../interpreter interpreter.hs

.PHONY: clean all

clean:
		rm -f interpreter

		rm -f src/*.o
		rm -f src/*.hi
		rm -f src/Makefile

		rm -f src/TypeChecker/*.o
		rm -f src/TypeChecker/*.hi

		rm -f src/Syntax/*.hi
		rm -f src/Syntax/*.hs
		rm -f src/Syntax/*.bak
		rm -f src/Syntax/*.o
		rm -f src/Syntax/*.txt
		rm -f src/Syntax/*.x
		rm -f src/Syntax/*.info
		rm -f src/Syntax/*.y
		rm -f src/Syntax/TestSyntax
