grammar.dvi: grammar.tex
	latex grammar && latex grammar

grammar.ps: grammar.dvi
	dvips grammar -o grammar.ps

clean:
	-rm grammar.aux grammar.log grammar.toc
