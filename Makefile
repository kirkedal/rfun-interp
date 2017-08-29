FILES=src/Main.hs src/Jana/*.hs

all:
	(cd src; make opt)

web:
	(cd src; make web)
