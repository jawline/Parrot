all: build

build:
	mkdir -p ./bin/
	ghc --make -isrc -outputdir ./bin/ -o ./bin/main src/main.hs

test: build
	./bin/main < test/example.md

clean:
	rm -rf ./bin/
