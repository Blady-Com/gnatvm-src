all: include obj/Debug obj/Debug/assemblyinfo bin/Debug
	dotnet-gnatmake -Phello --RTS=compact

clean:
	dotnet-gnatclean -Phello
	rm -rf obj bin include

include:
	cil2ada -compact -quiet -r -o include System.Windows.Forms

obj/Debug:
	mkdir -p obj/Debug

obj/Debug/assemblyinfo:
	cp assemblyinfo obj/Debug

bin/Debug:
	mkdir -p bin/Debug
