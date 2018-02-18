@echo OFF
echo CREATING BINDINGS USED IN THIS PROJECT

if exist include\mssyst-windows-forms-form.ads goto end-include

cil2ada -compact -quiet -r -o include System.Windows.Forms

:end-include
