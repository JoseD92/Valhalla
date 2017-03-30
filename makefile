all: main.pdf

main.pdf: main.tex
	latex main.tex
	bibtex main.tex
	latex main.tex
	latex main.tex

clean:
	git clean -X -f

remake:
	make clean
	make