all: slides

SLIDES = Slides.lhs
#TYPECHECK = ghc -fno-code $(SLIDES)
TYPECHECK = cabal clean && cabal configure && cabal build
SLIDES_PDF = Slides.pdf

slides:
	pdflatex $(SLIDES)
	pdflatex $(SLIDES)
	pdflatex $(SLIDES)

typecheck: $(SLIDES)
	$(TYPECHECK)

spell: $(SLIDES)
	aspell check -len_GB $(SLIDES)

open: slides
	xdg-open $(SLIDES_PDF)

clean:
	cabal clean
	rm -rf *.pdf *.loc *.toc *.log *.idx *.aux *.out *.nav *.snm *.vrb

