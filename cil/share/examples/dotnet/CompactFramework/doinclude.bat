echo CREATING BINDINGS USED IN THIS PROJECT
cd include

if exist mssyst-windows-forms-form.ads goto end

cil2ada mscorlib.dll -compact -quiet
cil2ada System.dll -compact -quiet
cil2ada System.Drawing.dll -compact -quiet
cil2ada System.Windows.Forms.dll -compact -quiet

:end