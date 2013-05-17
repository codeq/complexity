vendor/language-py:
	git clone https://github.com/codeq/language-py.git vendor/language-py

deps: vendor/language-py
	cabal-dev add-source vendor/language-py
	cabal-dev install --only-dependencies

configure:
	cabal-dev configure

clean:
	rm -rf cabal-dev dist vendor

.PHONY: deps configure clean
