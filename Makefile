run:
	ghc -o latte generated/*.hs src/LLVM/*.hs frontend.hs  Main.hs
wc:
	find src -name '*.hs' | xargs wc -l
generate_grammar:
	mkdir -p generated && \
	cd generated && \
	bnfc -m ../grammar/Latte.cf
	cd generated && make && \
	rm TestLatte.hs

.PHONY: clean
clean:
	rm -rf generated
