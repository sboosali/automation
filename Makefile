##################################################
##################################################
all: build

##################################################
.PHONY:	all shell check configure build clean docs update rebuild

##################################################
##################################################
shell:
	nix-shell

##################################################
##################################################
configure:
	cabal --enable-nix new-configure --project-file ./cabal/spiros/cabal.project
# --with-compiler /nix/store/b7ap301ixakvk87mdhl4gdm2v5z49yzn-ghc-8.6.0.20180627/bin/ghc

########################################
check:
	cabal new-build -fno-code -O0 all

########################################
build: check
	cabal new-build all

########################################
clean:
	rm -rf dist/ dist-newstyle/
	rm -f *.project.local .ghc.environment.*

##################################################
########################################
docs:
	cabal new-haddock 
# 	cp -aRv dist-newstyle/build/*/*/unpacked-containers-0/doc/html/unpacked-containers/* docs
# 	cd docs && git commit -a -m "update haddocks" && git push && cd ..

##################################################
update:
	cabal update

########################################
rebuild: clean update configure build docs

########################################