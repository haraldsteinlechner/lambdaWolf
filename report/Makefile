MAIN= report
SRC= 

TEX_FILES=$(addsuffix .tex, $(SRC)) 

DVIPS= dvips
DVIPS_OPT=

PDFLATEX= pdflatex
PDFLATEX_OPT= -interaction nonstopmode -halt-on-error -file-line-error

LATEX= latex
LATEX_OPT=$(PDFLATEX_OPT)

BIBTEX= bibtex
BIBTEX_OPT=

GZIP=gzip
GZIP_OPT=-f

SPELLCHECK= aspell check
LANG= en_GB
PDFVIEWER= evince
RM=rm -f

CREATE_BACKGROUND=./create-background.sh
PDFTK=pdftk

FIGURES= 

.SUFFIXES:
	.PHONY: view clean distclean gen_figures

all: $(MAIN).pdf 

gen_figures:
		make -C figures/ $(addsuffix .eps, $(FIGURES)) $(addsuffix .pdf, $(FIGURES))

%.dvi: %.tex $(TEX_FILES) 
		$(LATEX) $(LATEX_OPT) $<
			if [ "`grep -e '^[:blank:]*\\\bibliography{.*}' $^`" != "" ] ; then \
			  	  $(BIBTEX) $(BIBTEX_OPT) $(MAIN) ;\
				  	else \
						  echo "No Bibliography command found" ; \
						  	fi 
				$(LATEX) $(LATEX_OPT) $<
					$(LATEX) $(LATEX_OPT) $<

%.ps: %.dvi
		$(DVIPS) $(DVIPS_OPT) $< -o $@

%.ps.gz: %.ps
		$(GZIP) $(GZIP_OPT) $< > $@

%.pdf: %.tex $(TEXFILES) 
		$(PDFLATEX) $(PDFLATEX_OPT) $<
			  	  $(BIBTEX) $(BIBTEX_OPT) $(MAIN) ;\
				$(PDFLATEX) $(PDFLATEX_OPT) $<
					$(PDFLATEX) $(PDFLATEX_OPT) $< 

spellcheck:
		for file in $(MAIN).tex $(TEX_FILES) ; do \
		  	  $(SPELLCHECK) -t -d $(LANG) --personal=./$(MAIN).wordlist --repl=./$(MAIN).repllist $$file ; \
			  	  $(RM) $$file.bak ; \
				  	done

background_%.tex: %.pdf
		$(CREATE_BACKGROUND) $< $@

%.hash.pdf: %.pdf background_%.pdf
		$(PDFTK) $< multibackground background_$< output $@

view: all
		$(PDFVIEWER) $(MAIN).pdf &

clean:
		$(RM) $(addprefix $(MAIN).,toc aux pdf ps eps log lof bbl blg dvi out ps.gz)
			$(RM) $(addprefix $(MAIN).hash.,toc aux pdf ps eps log lof bbl blg dvi out ps.gz)
				rm -f *.aux 


