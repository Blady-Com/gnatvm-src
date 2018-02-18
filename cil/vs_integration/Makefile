MKDIR=mkdir -p
CP=cp -p
MSBUILD=msbuild.exe
INSTALLDIR=$(shell pwd)/install

.PHONY: all install clean

all: vs_lexer/bin/Release/vs_lexer.dll
	$(MSBUILD) AdaProject/AdaProject.csproj /p:Configuration=Release /t:Build
	$(MSBUILD) CompileTask/AdaCompileTask.csproj /p:Configuration=Release /t:Build

install:
	$(MKDIR) $(INSTALLDIR)
	$(CP) vs_lexer/bin/Release/vs_lexer.dll $(INSTALLDIR)
	$(CP) CompileTask/bin/Release/AdaCompileTask.dll $(INSTALLDIR)
	$(CP) AdaProject/bin/Release/AdaPackage.dll $(INSTALLDIR)
	$(MKDIR) $(INSTALLDIR)/Resources
	$(CP) CompileTask/DotGnat.targets $(INSTALLDIR)/Resources/
	$(MKDIR) $(INSTALLDIR)/Templates/ProjectItems
	$(CP) AdaProject/Templates/ProjectItems/*.ad[bs] $(INSTALLDIR)/Templates/ProjectItems
	$(MKDIR) $(INSTALLDIR)/Templates/Projects
	$(CP) -r AdaProject/Templates/Projects/* $(INSTALLDIR)/Templates/Projects

vs_lexer/bin/Release/vs_lexer.dll:
	$(MAKE) -C vs_lexer

clean:
	$(MSBUILD) AdaPackage.sln /t:Clean /p:Configuration=Release
	-$(MAKE) -C vs_lexer clean
