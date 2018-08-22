##################################################
##################################################
all: build

####################
.PHONY:	all check configure build clean docs update rebuild

##################################################
##################################################
configure:
	cabal new-configure --project-file ../cabal.project

####################
check:
	cabal new-build -fno-code -O0 xdotool

####################
compile:
	cabal new-build xdotool

####################
repl:
	cabal new-repl xdotool

# ####################
# install:
# 	cabal new-build xdotool

####################
rebuild: clean update configure build docs

####################
clean:
	rm -rf dist/ dist-newstyle/ .sboo/
	rm -f *.project.local .ghc.environment.*

##################################################
##################################################
build: check compile

####################
tags: compile
	mkdir -p .sboo/
	fast-tags -o ".sboo/tags" -R .
	cat ".sboo/tags"

########################
build-docs: compile
	cabal new-haddock xdotool

########################
copy-docs: build-docs
	rm -fr ".sboo/documentation/"
	mkdir -p ".sboo/documentation/"
	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/xdotool-*/noopt/doc/html/xdotool/* ".sboo/documentation/"

########################
open-docs: copy-docs
	xdg-open ".sboo/documentation/index.html"

#       ^ TODO: cross-platform, use 'open' and alias it to 'xdg-open'; wildcard the platform-directory (and also the various versions), i.e.:
#
#           open ./dist-newstyle/build/*-*/ghc-*/*-*/noopt/doc/html/*/index.html
#       rather than, e.g.:
#
#           xdg-open ./dist-newstyle/build/x86_64-linux/ghc-8.4.3/xdotool-0.0/noopt/doc/html/xdotool/index.html
#

# ekmett:
# 	cp -aRv dist-newstyle/build/*/*/unpacked-containers-0/doc/html/unpacked-containers/* docs
# 	cd docs && git commit -a -m "update haddocks" && git push && cd ..

########################
docs: open-docs

####################
update:
	cabal new-update

####################
watch:
	@exec ./scripts/sboo/ghcid.sh & disown

##################################################
##################################################