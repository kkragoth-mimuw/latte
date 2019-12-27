run:
	ghc -o lakke generated/*.hs src/Typechecker/*.hs src/Interpreter/*.hs src/Interpreter/Semantics/*.hs  Main.hs
wc:
	find src -name '*.hs' | xargs wc -l
generate_grammar:
	mkdir -p generated && \
	cd generated && \
	bnfc -m ../grammar/Latte.cf
	cd generated && make

.PHONY: clean
clean:
	rm -rf generated
