CC=ocamlc
LDFLAGS=-I /ocamlgraph/ graph.cma
CFLAGS=$(LDFLAGS)
EXEC=dag_test

all: $(EXEC)

run: $(EXEC)
	./$(EXEC)

dag_test: dag.cmo dag_test.cmo
	$(CC) $(LDFLAGS) -o $@ $^

main: dag.cmo main.cmo
	$(CC) $(LDFLAGS) -o $@ $^

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
