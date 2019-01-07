all: build

build:
	mkdir -p ./bin/
	ghc --make -outputdir ./bin/ -o ./bin/main src/main.hs

test: build
	./bin/main < test/example.md

clean:
	rm -rf ./bin/
