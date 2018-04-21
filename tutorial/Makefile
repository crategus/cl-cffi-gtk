all: html pdf

html:
	makeinfo --html --css-include=manual.css --split=chapter gtk-tutorial.texi
	cp -R figures/ gtk-tutorial/

pdf:
	texi2pdf gtk-tutorial.texi

clean:
	rm -rf gtk-tutorial/
	rm -f gtk-tutorial.pdf gtk-tutorial.fns gtk-tutorial.toc \
		gtk-tutorial.fn gtk-tutorial.aux gtk-tutorial.log
