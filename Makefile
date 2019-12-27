run:
	ghc -o lakke generated/*.hs src/Frontend/Typechecker/*.hs  Main.hs
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
