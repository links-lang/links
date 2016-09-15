all: quick-help.html

grammar.pdf: grammar.tex
	pdflatex grammar && pdflatex grammar

grammar.dvi: grammar.tex
	latex grammar && latex grammar

grammar.ps: grammar.dvi
	dvips grammar -o grammar.ps

clean:
	-rm -f grammar.aux grammar.log grammar.toc grammar.dvi grammar.pdf grammar.ps quick-help.html

quick-help.html: quick-help.pod
	pod2html quick-help.pod > quick-help.html
	@rm pod2htm*.tmp

