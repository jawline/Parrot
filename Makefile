all: build

build:
	mkdir -p ./bin/
	ghc --make -O3 -isrc -outputdir ./bin/ -o ./bin/main src/main.hs

test: build
	./bin/main 
	(cd bin; python ../scripts/test_server.py)

clean:
	rm -rf ./bin/
