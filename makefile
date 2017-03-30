all: main.pdf

main.pdf: main.tex
	latex main.tex

clean:
	find . -type f -name .gitignore -exec rm {} \;