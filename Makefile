SHELL := /bin/bash


output_dir  = /tmp/online_atps
tests_path = test/basics


OnlineATPs = dist/build/online-atps/online-atps

.PHONY: TAGS
TAGS :
	fast-tags -R -e src/

.PHONY : errors
errors :
	shelltest --color --execdir --precise --extension=.test  $(tests_path)/
	@echo "$@ succeeded!"

# Hlint test

# Requires HLint >= 1.9.36 and run `cabal build` or `cabal install`
# before.
.PHONY : hlint
hlint :
	hlint --color=never Setup.hs
	hlint --color=never \
              --cpp-file=dist/build/autogen/cabal_macros.h \
              --cpp-include=src/OnlineATPs/ \
              src/
	@echo "$@ succeeded!"

.PHONY : haddock
haddock :
	cabal configure
	cabal haddock --executables \
	              --haddock-option=--use-unicode \
	              --hyperlink-source
	@echo "$@ succeeded!"

.PHONY : install-bin
install-bin :
	cabal install --disable-documentation

.PHONY : install-fix-whitespace
install-fix-whitespace :
	cd src/fix-whitespace && cabal install


.PHONY : check-whitespace
check-whitespace :
	fix-whitespace --check

.PHONY : TODO
TODO :
	find . -type d \( -path './.git' -o -path './dist' \) -prune -o -print \
	| xargs grep -I 'TODO' \
	| sort

.PHONY : clean
clean :
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.o' -delete
	find . -type f -name 'atps.txt' -delete
	rm -f -r $(output_dir)

.PHONY : tests
tests :
	make errors
	make hlint
	make check-whitespace
	make haddock
	@echo "$@ succeeded!"
