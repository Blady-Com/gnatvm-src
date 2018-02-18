MAKEINFO=makeinfo
TEXI2DVI=texi2dvi
UG      =dotgnat_ug
CP      =cp -p
MKDIR   =mkdir -p
PREFIX  =

all: html txt pdf clean-temp-files

install:
ifneq ($(PREFIX),)
	$(MKDIR) $(PREFIX)/share/doc/gnat/txt
	$(MKDIR) $(PREFIX)/share/doc/gnat/html
	$(MKDIR) $(PREFIX)/share/doc/gnat/pdf
	$(CP) $(UG).txt $(PREFIX)/share/doc/gnat/txt
	$(CP) $(UG).html $(PREFIX)/share/doc/gnat/html
	$(CP) $(UG).pdf $(PREFIX)/share/doc/gnat/pdf
else
	@echo "PREFIX not set, cannot install."
	@echo "Usage: make PREFIX=/path/to/prefix install"
endif

html:
	$(MAKEINFO) --html --no-split --number-sections $(UG).texi -o $(UG).html

txt:
	$(MAKEINFO) --force --no-headers --no-split $(UG).texi -o $(UG).txt

pdf:
	echo x | $(TEXI2DVI) -p $(UG).texi

clean-temp-files:
	-$(RM) *.aux *.cp *.cps *.fn *.ky *.log *.pg *.toc *.tp *.vr
