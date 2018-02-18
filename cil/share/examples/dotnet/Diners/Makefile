all: bin/Debug/AdaLib.dll
	csc /r:AdaLib/bin/Debug/AdaLib.dll /out:bin/Debug/Diners.exe Program.cs

bin/Debug/AdaLib.dll: AdaLib/bin/Debug/AdaLib.dll
	mkdir -p bin/Debug
	cp AdaLib/bin/Debug/AdaLib.dll bin/Debug

AdaLib/bin/Debug/AdaLib.dll:
	$(MAKE) -C AdaLib lib

clean:
	rm -rf obj bin
	$(MAKE) -C AdaLib clean

