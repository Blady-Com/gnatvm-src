# Makefile for GNAT for .NET
# Copyright (C) 2006-2011, AdaCore

TARGET=cil
TARGETLANG=CIL
DEF_TARGET=CLI_Target
NON_ADA_RT_FILES=*.cs
objext=.il

# Make sure that those variables are initialized before including
# Makefile.common

ifeq ($(RTS),)
  LIBDIR=dotgnat
  ADALIB=ada
  MSCORLIB=mscorlib
  GNATLIB=gnatlib
  GNATKEY=gnatkey.snk
  SYSTEMLIB=
else
  LIBDIR=dotgnat/rts-$(RTS)
  ADALIB=ada$(RTS)_
  MSCORLIB=mscorlib-$(RTS)
  GNATLIB=gnatlib-$(RTS)
  GNATKEY=gnatkey-$(RTS).snk
  CSCDEFS=/D:COMPACT /D:$(shell echo $(RTS) | tr [a-z] [A-Z])
  EXTRA_EXCLUDE_GNATRTL_OBJS=g-ctrl_c% a-direio%
ifeq ($(RTS),compact)
  SYSTEMLIB:=$(strip $(shell reg query HKLM\\SOFTWARE\\Microsoft\\.NETCompactFramework\\v2.0.0.0\\WindowsCE\\AssemblyFoldersEx /ve | iconv -c | grep REG_SZ | sed -e 's/.*REG_SZ *//'))
else
  SYSTEMLIB:=$(strip $(shell reg query HKLM\\SOFTWARE\\Microsoft\\Microsoft\ SDKs\\Silverlight\\v3.0\\ReferenceAssemblies /v SLRuntimeInstallPath | grep REG_SZ | sed -e 's/.*REG_SZ *//'))
endif
endif

EXTRA_GNATRTL_OBJS=i-cil$(objext) i-cilobj$(objext) i-cilthr$(objext) \
  i-cithpr$(objext) i-cithth$(objext) cil_types$(objext)

GNATLIB_PATCH=cilruntime.difs
TOOLS_PREFIX=dotnet-
GNATCOMPILE=dotnet-gnatcompile
SHORT_GNATCOMPILE=gnatcompile
GNATCMD=gnat

ifeq ($(OS),Windows_NT)
  TARGET_TOOLS=dotnet-ld cil2ada
else
  TARGET_TOOLS=dotnet-ld
endif

-include ../Makefile.common

.PHONY: cs.il

sanity-check:
ifeq (,$(findstring $(TARGET),$(target)))
	@echo "You must run make in the src/$(TARGET) directory"
	@exit 1
endif
	@if [ ! -f $(GNAT_SRC)/Makefile.rtl ]; then \
	  echo "$(GNAT_SRC)/Makefile.rtl not found"; exit 1; \
	fi

libgnat: $(lib)/adainclude/mscorlib.txt $(lib)/adainclude/gnatlib.txt $(GNATRTL_OBJS)
	$(MAKE) mgnat

# .NET internal section
ifeq ($(OS),Windows_NT)
  CSC=csc /nologo
  ILASM=ilasm /debug /quiet
  ILDASM=ildasm /linenum
  ILDASMOUTPUT=/out:
  GACUTIL_I=gacutil /nologo /if
else
  CSC=gmcs /D:MONO
  ILASM=ilasm /debug /quiet
  ILDASM=monodis
  ILDASMOUTPUT=--output=
  GACUTIL_I=gacutil /i
endif

ifeq ($(DEBUG),yes)
  EXTRACSOPTS=/debug+
else
  EXTRACSOPTS=/optimize+
endif

CSCOPTS=$(EXTRACSOPTS) /nostdlib+ /noconfig /checked+ /reference:"$(SYSTEMLIB)mscorlib.dll" /reference:"$(SYSTEMLIB)System.dll"
ifeq ($(RTS),)
  CSCOPTS+=/reference:"$(SYSTEMLIB)System.Windows.Forms.dll"
endif

GNAT_VERSION:=$(shell cat $(GNAT_SRC)/gnatvsn.ads | grep "  Gnat_Static_Version_String" | sed -e 's/[^"]*\"\([^"]*\)\".*/\1/')
HAS_DOTS:=$(shell echo "$(GNAT_VERSION)" | grep [.])
ifeq ($(HAS_DOTS),)
  GNATLIB_VER:=$(shell echo "$(GNAT_VERSION)" | sed -e 's/\([0-9.]*\).*/\1/').0.0.0
else
  GNATLIB_VER:=$(shell echo "$(GNAT_VERSION)" | sed -e 's/\([0-9.]*\).*/\1/').0
endif
GNATLIB_VER_IL=$(subst .,:,$(GNATLIB_VER))

CUR_YEAR=$(shell cat $(GNAT_SRC)/gnatvsn.ads | grep " Current_Year" | sed -e 's/[^"]*\"\([0-9]*\)\".*/\1/')

vpath %.cs.in $(gnatlib)
vpath %.cs $(lib)/adainclude

AssemblyInfo.cs: AssemblyInfo.cs.in
	cat $< | sed -e 's/@GNATLIB_VER@/$(GNATLIB_VER)/' -e 's/@CUR_YEAR@/$(CUR_YEAR)/' > $(lib)/adainclude/$@

# convert cs files into il: first create a dll from c# files only, then
# decompile it into a .il file. We can then use the .il file to create the
# finnal library.

$(lib)/adainclude/mscorlib.txt: $(gnatlib)/$(MSCORLIB).txt
	$(CP) $< $@

$(lib)/adainclude/gnatlib.txt: $(gnatlib)/$(GNATLIB).txt
	cat $< | sed -e 's/@GNATLIB_VER@/$(GNATLIB_VER_IL)/' > $@

mgnat: $(GNATRTL_OBJS) AssemblyInfo.cs
	cd $(lib)/adalib; \
	$(CSC) $(CSCDEFS) $(CSCOPTS) /t:library /keyfile:$(GNATKEY) \
	  /out:mgnat.dll ..$(DIRSEP)adainclude$(DIRSEP)*.cs
ifneq ($(RTS),silverlight)
	cd $(lib)/adalib; $(GACUTIL_I) mgnat.dll
endif

cil2ada:
	$(MAKE) -C tools/cil2ada

dotnet-ld:
	$(MAKE) -C tools/ld

compactlib:
	$(MAKE) RTS=compact gnatlib

quickcompactlib:
	$(MAKE) RTS=compact quicklib
