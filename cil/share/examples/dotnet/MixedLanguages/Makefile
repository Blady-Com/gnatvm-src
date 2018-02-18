all: AdaLib/bin/Debug AdaLib/obj/Debug
	cd AdaLib; dotnet-gnatmake -PAdaLib
	cp AdaLib/bin/Debug/AdaLib.dll CSharpApp
	cd CSharpApp; csc /target:exe /out:csharpapp.exe Properties\\AssemblyInfo.cs Program.cs /r:AdaLib.dll
	CSharpApp/csharpapp.exe

clean:
	rm -f CSharpApp/*.exe CSharpApp/*.dll
	rm -rf AdaLib/bin AdaLib/obj

AdaLib/bin/Debug:
	mkdir -p AdaLib/bin/Debug

AdaLib/obj/Debug:
	mkdir -p AdaLib/obj/Debug