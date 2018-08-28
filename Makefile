##################################################
##################################################
build: check compile

#------------------------------------------------#

all: build docs tarball upload
#all: build docs tarball upload

#------------------------------------------------#

.PHONY:	all shell check configure build clean docs update rebuild

##################################################
##################################################
shell:
	nix-shell

##################################################
##################################################
configure:
	cabal --enable-nix new-configure --project-file ./cabal/spiros/cabal.project

#------------------------------------------------#
compile:
	cabal new-build all

#------------------------------------------------#
repl:
	cabal new-repl xdotool

#------------------------------------------------#
#install:
#	cabal new-build all

#------------------------------------------------#
check:
	cabal new-build -fno-code -O0 all

##################################################
##################################################
clean:
	rm -rf dist/ dist-newstyle/ .sboo/
	rm -f *.project.local .ghc.environment.*

##################################################
##################################################
build-docs: compile
	cabal new-haddock all

#------------------------------------------------#
copy-docs: build-docs
	rm -fr ".sboo/documentation/"
	mkdir -p ".sboo/documentation/"
	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/automation-*/noopt/doc/html/automation/* ".sboo/documentation/"

#------------------------------------------------#
open-docs: copy-docs
	xdg-open ".sboo/documentation/index.html"

#------------------------------------------------#
docs: open-docs

##################################################
##################################################
watch:
	@exec ghcid
#	@exec ./scripts/sboo/ghcid.sh & disown

#------------------------------------------------#

##################################################
##################################################
tags: compile
	mkdir -p .sboo/
	fast-tags -o ".sboo/tags" -R .
	cat ".sboo/tags"

##################################################
##################################################
tarball:

	(cd automation-for-x11-via-shell && cabal sdist)

	mkdir -p ".sboo/tarballs"
	mv automation-for-x11-via-shell/dist/automation-for-x11-via-shell-*.tar.gz ".sboo/tarballs"

	find ".sboo/tarballs"

#------------------------------------------------#

upload: tarball
#	cabal TODO

##################################################
##################################################
update:
	cabal new-update

##################################################
# NOTES ##########################################
##################################################

#       ^ TODO: cross-platform, use 'open' and alias it to 'xdg-open'; wildcard the platform-directory (and also the various versions), i.e.:
#
#           open ./dist-newstyle/build/*-*/ghc-*/*-*/noopt/doc/html/*/index.html
#       rather than, e.g.:
#
#           xdg-open ./dist-newstyle/build/x86_64-linux/ghc-8.4.3/automation-0.0/noopt/doc/html/automation/index.html
#

# ekmett:
# 	cp -aRv dist-newstyle/build/*/*/unpacked-containers-0/doc/html/unpacked-containers/* docs
# 	cd docs && git commit -a -m "update haddocks" && git push && cd ..

##################################################
##################################################