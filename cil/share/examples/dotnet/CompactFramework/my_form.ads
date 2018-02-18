with CIL_Types;
with MSSyst;                           use MSSyst;
with MSSyst.Drawing.Color;
with MSSyst.EventArgs;
with MSSyst.Object;
with MSSyst.String;                    use MSSyst.String;
with MSSyst.Windows.Forms;             use MSSyst.Windows.Forms;
with MSSyst.Windows.Forms.Application;
with MSSyst.Windows.Forms.Form;
with MSSyst.Windows.Forms.Label;
with MSSyst.Windows.Forms.MainMenu;
with MSSyst.Windows.Forms.MenuItem;

package My_Form is

   type My_Form_Record is new MSSyst.Windows.Forms.Form.Typ with record
      Main_Menu    : MainMenu.Ref;
      Exit_Menu    : MenuItem.Ref;
      Action_Menu  : MenuItem.Ref;
      Some_Text    : Label.Ref;
      Color        : MSSyst.Drawing.Color.ValueType;
   end record;
   type My_Form is access all My_Form_Record'Class;

   function new_Form
     (This : My_Form := null) return My_Form;
   pragma CIL_Constructor (new_Form);

   procedure Initialize_Components
     (This : access My_Form_Record);

   The_Form : My_Form;

end My_Form;


