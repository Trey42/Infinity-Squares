test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

gui:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte

clean:
	ocamlbuild -clean

zip:
	zip src.zip *
