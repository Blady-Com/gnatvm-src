MKDIR=mkdir -p
GNATMAKE=dotnet-gnatmake
GNATCLEAN=dotnet-gnatclean
CP=cp -p

all: setup mscorlib
	$(GNATMAKE) -Pvs_lexer

setup:
	$(MKDIR) include lib obj/Release bin/Release
	$(CP) Properties/assemblyinfo obj

mscorlib:
	cil2ada -q mscorlib -o include

clean:
	-rm -rf obj bin lib include
