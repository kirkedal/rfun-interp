FILES=Ast.hs Interp.hs Main.hs Parser.hs Preparse.hs

rfun: $(FILES)
	ghc --make  -Wall -fno-warn-unused-do-bind Main.hs

optimized: $(FILES)
	ghc -O2 -rtsopts --make Main.hs

haddock h: $(FILES)
	haddock -h -o ../docs/ Main.hs

clean:
	rm -f $(subst .hs,.o,$(FILES))
	rm -f $(subst .hs,.hi,$(FILES))
	rm -f Main

test:
	(cd ../test; sh MyTest.txt)
