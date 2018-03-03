# Makefile for JGNAT
# Copyright (C) 2006, AdaCore

TARGET=jvm
TARGETLANG=Java
DEF_TARGET=JVM_Target
LIBDIR=jgnat
NON_ADA_RT_FILES=*.java
objext=.class

EXTRA_GNATRTL_OBJS=i-jalaob$(objext) i-jalasy$(objext) i-jalath$(objext) \
  i-java$(objext) i-javlan$(objext) java$(objext)

EXTRA_EXCLUDE_GNATRTL_OBJS=a-envvar%

GNATRTL_COMMON_SRCS=\
 a-einuoc.adb \
 a-excpol.adb \
 a-stream.ads \
 a-ststio.adb \
 a-tags.ads \
 a-taside.adb \
 a-tigeli.adb \
 i-cstrea.adb \
 s-addima.adb \
 s-assert.adb \
 s-fatgen.adb \
 s-fileio.adb \
 s-finimp.ads \
 s-finimp.adb \
 s-finmas.ads \
 s-finmas.adb \
 s-finroo.ads \
 s-inmaop.ads \
 s-intman.ads \
 s-osprim.ads \
 s-parame.ads \
 s-purexc.ads \
 s-secsta.ads \
 s-secsta.adb \
 s-stalib.ads \
 s-stausa.ads \
 s-stausa.adb \
 s-stoele.adb \
 s-tasdeb.adb \
 s-tataat.adb

TOOLS_PREFIX=jvm-
GNATCOMPILE=jvm-gnatcompile
SHORT_GNATCOMPILE=gnatcompile
GNATCMD=gnat
EXTRA_TOOLS=jvm2ada jvmlist jvmstrip jarmake javastub

-include ../Makefile.common

sanity-check:
ifeq (,$(findstring $(TARGET),$(target)))
	@echo "You must run make in the src/$(TARGET) directory"
	@exit 1
endif
	@if [ ! -f $(GNAT_SRC)/Makefile.rtl ]; then \
	  echo "$(GNAT_SRC)/Makefile.rtl not found"; exit 1; \
	fi

# JVM internal section

JAVAC=javac -source 1.5 -target 1.5 -deprecation -classpath $(target)/lib
JAR=jar c0f
JGNAT_JAR=jgnat.jar

ifeq ($(OS),Windows_NT)
  CLASS_FILES=jgnat/adalib/\*.class
  BIND_JRE=bind_jre.cmd
else
  CLASS_FILES=jgnat/adalib/*.class
  BIND_JRE=bind_jre.sh
endif

libgnat: $(GNATRTL_OBJS) java_objs

java_objs:
	$(JAVAC) $(lib)/adainclude/*.java -d $(target)/lib
ifneq ($(LIBDIR),jgnat)
	$(MV) $(target)/lib/jgnat/adalib $(target)/lib/jgnat/adalib-full
	$(MV) $(target)/lib/$(LIBDIR)/adalib $(target)/lib/jgnat/adalib
endif
	cd $(target)/lib; $(JAR) $(JGNAT_JAR) $(CLASS_FILES)
ifneq ($(LIBDIR),jgnat)
	$(MV) $(target)/lib/jgnat/adalib $(target)/lib/$(LIBDIR)
	$(MV) $(target)/lib/jgnat/adalib-full $(target)/lib/jgnat/adalib
endif

$(EXTRA_TOOLS): cfiles force
	cd $(obj); $(HOST_MAKE) $@ -o $(bin)/$@
	$(CP) $(BIND_JRE) $(bin)

ZFP_COMMON_SRCS=\
 a-tags.ads s-assert.adb s-secsta.ads s-secsta.adb s-stoele.adb s-fatgen.adb

NON_ADA_ZFP_FILES=\
 Lng.java Dbl.java Flt.java Int.java \
 standard*.java Ada_AR.java Acc.java \
 constraint_error.java program_error.java storage_error.java

ZFP_JVM_FILES=a-except.ad? a-tags.adb

ZFP_TARGET_PAIRS=\
 s-assert.adb<s-assert-xi.adb \
 g-io.ads<g-io-zfp.ads \
 g-io.adb<g-io-zfp.adb \
 i-c.ads<i-c-hie.ads

setupzfplib:
	@echo setting up adainclude for zfp
	-$(RM) $(lib)/rts-zfp/adainclude/* $(lib)/rts-zfp/adalib/*
	$(MKDIR) $(lib)/rts-zfp/adainclude $(lib)/rts-zfp/adalib
	-@cd $(GNAT_SRC); $(CP) $(ZFP_SRCS) $(lib)/rts-zfp/adainclude 2> /dev/null
	$(foreach PAIR,$(ZFP_TARGET_PAIRS), \
	  $(CP) $(GNAT_SRC)/$(word 2,$(subst <, ,$(PAIR))) \
	    $(lib)/rts-zfp/adainclude/$(word 1,$(subst <, ,$(PAIR)));)
	-@cd $(gnatlib_common); $(CP) $(ZFP_COMMON_SRCS) $(lib)/rts-zfp/adainclude > /dev/null
	@cd $(gnatlib); $(CP) $(NON_ADA_ZFP_FILES) $(ZFP_JVM_FILES) $(lib)/rts-zfp/adainclude
	@sed -e 's/Configurable_Run_Time.*/Configurable_Run_Time     : constant Boolean := True;/' \
	    -e 's/Suppress_Standard_Library.*/Suppress_Standard_Library : constant Boolean := True;/' \
	  $(gnatlib)/system.ads > $(lib)/rts-zfp/adainclude/system.ads
	@echo "pragma No_Body;" > $(lib)/rts-zfp/adainclude/i-c.adb

zfplib: setupzfplib
	$(MAKE) LIBDIR="jgnat/rts-zfp" \
	  GNATRTL_OBJS="$(ZFP_OBJS)" \
	  JGNAT_JAR=jgnat_zfp.jar    \
	  quicklib readonly-lib

