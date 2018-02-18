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
  CSC=csc
  DIRSEP=\\
else
  CSC=gmcs
  DIRSEP=/
endif

all: cil2adavsn.cs
	$(CSC) /nologo /platform:x86 /nowarn:1607 $(EXTRACSCOPTS) /unsafe /out:..$(DIRSEP)..$(DIRSEP)bin$(DIRSEP)cil2ada.exe /recurse:*.cs

cil2adavsn.cs: cil2adavsn.cs.in $(GNATVSN)
	cat cil2adavsn.cs.in | sed -e 's/@VERSION_STRING@/$(GNATLIB_VER_STR)/' -e 's/@BUILD_TYPE@/$(GNATLIB_BUILD)/' -e  's/@LIBRARY_VERSION@/$(GNATLIB_VER)/' -e  's/@CURRENT_YEAR@/$(GNATLIB_YEAR)/' > cil2adavsn.cs

clean:
	rm -rf bin obj ../bin/cil2ada cil2adavsn.cs
