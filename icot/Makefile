LATEXMK := latexmk
TEXCOUNT := texcount
BUILD_FLAGS := -pdf -dvi- -ps-

all: reference.pdf

wordcount:
	texcount -merge report.tex

clean:
	$(LATEXMK) -C

.PHONY: all clean FORCE_MAKE

%.pdf: %.tex FORCE_MAKE
	$(LATEXMK) $(BUILD_FLAGS) $<
