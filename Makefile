all: build

build:
	mkdir -p ./bin/
	ghc --make -isrc -outputdir ./bin/ -o ./bin/main src/main.hs

test: build
	./bin/main

clean:
	rm -rf ./bin/
