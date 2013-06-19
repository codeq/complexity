.cabal-sandbox:
	cabal sandbox init

vendor/language-py:
	git clone https://github.com/codeq/language-py.git vendor/language-py

deps: .cabal-sandbox vendor/language-py
	cabal sandbox add-source vendor/language-py
	cabal install --only-dependencies

configure: .cabal-sandbox
	cabal configure

clean:
	rm -rf .cabal-sandbox cabal.sandbox.config dist vendor

.PHONY: deps configure clean
