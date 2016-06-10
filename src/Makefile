CC=ocamlc
INTERP=ledit ocaml
OCAMLGRAPH_LIB=/ocamlgraph/
LDFLAGS=-I $(OCAMLGRAPH_LIB) graph.cma
CFLAGS=$(LDFLAGS)
EXEC=main_test
EXECS=dag_test main_test

all: $(EXECS)

test: $(EXEC)
	./$(EXEC)

dag_test: dag.cmo dag_test.cmo
	$(CC) $(LDFLAGS) -o $@ $^

main_test: dag.cmo dag_test.cmo main.cmo main_test.cmo
	$(CC) $(LDFLAGS) -o $@ $^

debug: dag.cmo dag_test.cmo main.cmo
	$(INTERP) $(LDFLAGS) $^

#main: dag.cmo main.cmo
#	$(CC) $(LDFLAGS) -o $@ $^

main.cmo: main.cmi

main.cmi: main.mli
	$(CC) $(CFLAGS) -c $<

%.cmo: %.ml
	$(CC) $(CFLAGS) -c $<

.PHONY: clean mrproper

clean:
	rm -rf *.cmo *.cmi *.dot

mrproper: clean
	rm -rf $(EXECS)
