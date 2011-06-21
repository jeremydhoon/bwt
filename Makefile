#!/usr/bin/make

HASKELL = ghc
OPTHASKELL = -O2 -funbox-strict-fields
OPTC = -fvia-C -optc-O6 -optc-mtune=native -optc-O3 -optc-funroll-loops
TARGET = bwt
TESTTARGET = testbwt
MAIN = Main.hs
TESTMAIN = TestBwt.hs
TESTMAINFUNC = TestBwt.main
PROF = -prof -auto-all

all:
	$(HASKELL) $(OPTHASKELL) -o $(TARGET) --make $(MAIN)

prof:
	$(HASKELL) $(OPTHASKELL) $(PROF) -o $(TARGET) --make $(MAIN)

copt:
	$(HASKELL) $(OPTHASKELL) $(OPTC) -o $(TARGET) --make $(MAIN)

clean:
	rm *.o *.hi
test:
	$(HASKELL) -o $(TESTTARGET) $(PROF) -main-is $(TESTMAINFUNC) --make $(TESTMAIN)