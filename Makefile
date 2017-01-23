SHELL := /bin/bash


output_dir  = /tmp/online_atps
errors_path = test/fail/errors


OnlineATPs = dist/build/online-atps/online-atps


.PHONY : errors
errors :
	:shelltest --color --execdir --precise  $(errors_path)/errors.test
	@echo "$@ succeeded!"


.PHONY : haddock
haddock :
	cabal configure
	cabal haddock --executables \
	              --haddock-option=--use-unicode \
	              --hyperlink-source
	@echo "$@ succeeded!"

.PHONY : hlint
hlint :
	hlint --color=never Setup.hs
	hlint --color=never \
              --cpp-file=dist/build/autogen/cabal_macros.h \
              --cpp-include=src/OnlineATPs/ \
              src/

.PHONY : install-bin
install-bin :
	cabal install --disable-documentation

.PHONY : TODO
TODO :
	find . -type d \( -path './.git' -o -path './dist' \) -prune -o -print \
	| xargs grep -I 'TODO' \
	| sort

.PHONY : clean
clean :
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.o' -delete
	rm -f -r $(output_dir)