vendor/language-python:
	git clone https://github.com/codeq/language-python.git vendor/language-python

deps: vendor/language-python
	cabal-dev add-source vendor/language-python
	cabal-dev install --only-dependencies

configure:
	cabal-dev configure

clean:
	rm -rf cabal-dev dist vendor

.PHONY: deps configure clean
