all:
	ghc src/main.hs
	./src/main < test/example.md
