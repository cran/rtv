makerel:
	make makedev
	make clean

makedev:
	make rtv.pdf

rtv.pdf: rtv.tex maia2.sty
	pdflatex rtv.tex

rtv.tex: rtv.rnw
	R CMD Sweave rtv.rnw

clean:
	rm *.tex
	rm *.aux
	rm *.log
	rm tmp*
	rm Rplots*


