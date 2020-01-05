run:
	ghc -o latc_llvm src/generated/*.hs src/*.hs
wc:
	find src -name '*.hs' | xargs wc -l
generate_grammar:
	mkdir -p src/generated && \
	cd src/generated && \
	bnfc -m ../Latte.cf
	cd src/generated && make && \
	rm TestLatte.hs
.PHONY: clean
clean:
	rm -rf src/*.o src/*.hi
