all:
	cd libs && make clean && make
	cabal configure
	cabal new-build
	-cabal install --bindir=$(pwd)
	-cp latc latc_x86
	-cp dist-newstyle/build/x86_64-linux/ghc-8.4.4/latte-compiler-0.0.0.1/x/latc/build/latc/latc latc_x86

own-pc:
	cd libs && make clean && make own-pc
	cabal configure
	cabal build
	cabal install --bindir=$(pwd)
	cp latc latc_x86

clean:
	cabal clean
	-rm -f *.hi *.o
	make -C src clean
