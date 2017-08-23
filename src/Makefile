RFUNFILES=Ast.hs MainRFun.hs ParserRFun.hs PrettyPrinter.hs TypeCheck.hs Interp.hs
COREFILES=MainCore.hs Core/Ast.hs Core/Interp.hs Core/Parser.hs Core/Preparse.hs Core/RFun2Prog.hs
BNFC=rfun.cf

rfun: $(RFUNFILES)
	ghc --make -o rfun -Wall -fno-warn-unused-do-bind -XGeneralizedNewtypeDeriving MainRFun.hs

core: $(COREFILES)
	ghc --make -o rfunCore -Wall -fno-warn-unused-do-bind -XGeneralizedNewtypeDeriving MainCore.hs

optimized: $(RFUNFILES)
	ghc -O2 -rtsopts --make -XGeneralizedNewtypeDeriving MainRFun.hs

haddock h: $(RFUNFILES)
	haddock -h -o ../docs/ MainRFun.hs

clean:
	rm -f $(subst .hs,.o,$(RFUNFILES))
	rm -f $(subst .hs,.hi,$(RFUNFILES))
	rm -f rfun
	rm -f $(subst .hs,.o,$(COREFILES))
	rm -f $(subst .hs,.hi,$(COREFILES))
	rm -f rfunCore
	rm -f *.bak

test:
	(cd ../test; sh MyTest.txt)
