with AdaVisualStudio.AdaPackage;  use AdaVisualStudio.AdaPackage;
with AdaVisualStudio.AdaPackage.AdaDeclarations;
with AdaVisualStudio.AdaPackage.AdaMethods;

package Suggestions is

   procedure MakeNames
     (Buffer       : in Wide_String;
      Declarations : access Adadeclarations.Typ'Class);

end Suggestions;
