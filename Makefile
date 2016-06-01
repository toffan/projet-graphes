TEXC = latexmk -lualatex -output-directory=build -shell-escape

.PHONY: FORCE

all: projet.pdf

projet.pdf: projet.tex FORCE
	$(TEXC) $<

clean: FORCE
	$(TEXC) -c

clean_all: clean FORCE
	$(RM) build -r
