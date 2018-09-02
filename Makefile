main:
	ocamlbuild -r -pkgs oUnit,str,ANSITerminal test.byte && ./test.byte

test:
	ocamlbuild -r -pkgs oUnit,str,ANSITerminal test.byte && ./test.byte

play:
	ocamlbuild -r -pkgs str,ANSITerminal main.byte && ./main.byte

clean:
	ocamlbuild -r -clean
	rm -f checktypes.ml
