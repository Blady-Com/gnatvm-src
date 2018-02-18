GNATMAKE=dotnet-gnatmake

all: include/mssyst.ads obj/Debug/assemblyinfo obj/Debug/AdaLib.Properties.Resources.resources
	$(GNATMAKE) -p -PAdaLib -XBUILDTYPE=Standalone -XCONFIGURATION=Debug --RTS=compact

lib: include/mssyst.ads obj/Debug/assemblyinfo obj/Debug/AdaLib.Properties.Resources.resources
	$(GNATMAKE) -p -PAdaLib -XBUILDTYPE=Library -XCONFIGURATION=Debug --RTS=compact

clean:
	rm -rf obj bin include/*

include/mssyst.ads:
	cmd /c prebuild.bat

obj/Debug:
	mkdir -p $@

obj/Debug/assemblyinfo: obj/Debug
	cp assemblyinfo obj/Debug

obj/Debug/AdaLib.Properties.Resources.resources: Properties/Resources.resx
	cd Properties; ResGen Resources.resx ../obj/Debug/AdaLib.Properties.Resources.resources
