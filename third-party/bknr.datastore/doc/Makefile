PBOOK=../../tools/pbook.py
DEST=/home/bknr/public_html/pdf

all: manual.pdf

manual.pdf: manual.tex introduction.tex guidedtour.tex indices.tex datastore.tex impex.tex 
	pdflatex manual && pdflatex manual
	cp manual.pdf $(DEST)/datastore-manual.pdf

indices.tex: ../src/indices/tutorial.lisp
	$(PBOOK) -c BknrTexFile -t "BKNR Indices" -o indices.tex ../src/indices/tutorial.lisp

datastore.tex: ../src/data/tutorial.lisp
	$(PBOOK) -c BknrTexFile -t "BKNR Datastore" -o datastore.tex ../src/data/tutorial.lisp

impex.tex: ../src/xml-impex/tutorial.lisp
	$(PBOOK) -c BknrTexFile -t "BKNR XML Import/Export" -o impex.tex ../src/xml-impex/tutorial.lisp

clean:
	- rm -rf indices.tex manual.toc impex.tex guidedtour.aux manual.aux datastore.aux manual.pdf datastore.tex indices.aux introduction.aux impex.aux manual.log

