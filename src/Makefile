FILES=Ast.hs Interp.hs Main.hs Parser.hs Preparse.hs

rfun: $(FILES)
	ghc --make Main.hs

clean:
	rm -f $(subst .hs,.o,$(FILES))
	rm -f $(subst .hs,.hi,$(FILES))
	rm -f Main

test:
	(cd ../test; sh MyTest.txt)
