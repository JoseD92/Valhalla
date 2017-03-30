all: main.pdf

main.pdf: main.tex
	latex main.tex

clean:
	git clean -X -f