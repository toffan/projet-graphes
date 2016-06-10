CC=ocamlc
INTERP=ledit ocaml
OCAMLGRAPH_LIB=/ocamlgraph/
LDFLAGS=-I $(OCAMLGRAPH_LIB) graph.cma
CFLAGS=$(LDFLAGS)
EXEC=dag_test

all: $(EXEC)

run: $(EXEC)
	./$(EXEC)

dag_test: dag.cmo dag_test.cmo
	$(CC) $(LDFLAGS) -o $@ $^

main: dag.cmo main.cmo
	$(CC) $(LDFLAGS) -o $@ $^

test: dag.cmo main.cmo
	$(INTERP) $(LDFLAGS) $^

main.cmo: main.cmi

main.cmi: main.mli
	$(CC) $(CFLAGS) -c $<

%.cmo: %.ml
	$(CC) $(CFLAGS) -c $<

.PHONY: clean mrproper

clean:
	rm -rf *.cmo *.cmi *.dot

mrproper: clean
	rm -rf $(EXEC)
