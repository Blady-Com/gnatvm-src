GNAT_SRC=../../../gnat_src
GNATVSN=$(GNAT_SRC)/gnatvsn.ads

GNATLIB_VER=$(shell cat $(GNATVSN) | grep " Library_Version \:" | sed -e 's/[^"]*\"\([0-9.]*\)\".*/\1/')
GNATLIB_BUILD=$(shell cat $(GNATVSN) | grep " Build_Type \: constant Gnat_Build_Type" | sed -e 's/[^=]*= \([a-zA-Z]*\);.*/\1/')
GNATLIB_VER_STR=$(shell cat $(GNATVSN) | grep " Gnat_Static_Version_String \:" | sed -e 's/[^"]*\"\([^"]*\)\".*/\1/')
GNATLIB_YEAR=$(shell cat $(GNATVSN) | grep " Current_Year \:" | sed -e 's/[^"]*\"\([^"]*\)\".*/\1/')

ifeq ($(DEBUG),yes)
  EXTRACSCOPTS=/debug+
else
  EXTRACSCOPTS=/optimize+
endif

ifeq ($(OS),Windows_NT)
  CSC:=csc
  BINDIR:=..\\..\\bin\\
  INSTDIR:=$(BINDIR)
else
  CSC:=gmcs
  BINDIR:=../../bin/
  INSTDIR:=$(BINDIR).mono/
  MONO=yes
endif

all: ldvsn.cs
	@mkdir -p $(INSTDIR)
	$(CSC) /nologo $(EXTRACSCOPTS) /out:$(INSTDIR)dotnet-ld.exe /recurse:*.cs
ifeq ($(MONO),yes)
	install -m 0755 launcher $(BINDIR)dotnet-ld
endif
 
ldvsn.cs: ldvsn.cs.in $(GNATVSN)
	cat $< | sed -e 's/@VERSION_STRING@/$(GNATLIB_VER_STR)/' -e 's/@BUILD_TYPE@/$(GNATLIB_BUILD)/' -e  's/@LIBRARY_VERSION@/$(GNATLIB_VER)/' -e  's/@CURRENT_YEAR@/$(GNATLIB_YEAR)/' > $@

clean:
	rm -rf *.suo bin obj Properties ../bin/dotnet-ld ldvsn.cs
