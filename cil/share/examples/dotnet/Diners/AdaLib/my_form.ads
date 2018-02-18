with CIL_Types;
with MSSyst;                           use MSSyst;
with MSSyst.EventArgs;
with MSSyst.Object;
with MSSyst.Windows.Forms;             use MSSyst.Windows.Forms;
with MSSyst.Windows.Forms.Application;
with MSSyst.Windows.Forms.Form;
with MSSyst.Windows.Forms.MainMenu;
with MSSyst.Windows.Forms.MenuItem;
with MSSyst.Windows.Forms.Menu_MenuItemCollection;
with MSSyst.Windows.Forms.Timer;

package My_Form is

   ---------------
   -- MAIN FORM --
   ---------------

   type My_Form_Record is new MSSyst.Windows.Forms.Form.Typ with record
      Main_Menu    : MainMenu.Ref;
      Exit_Menu    : MenuItem.Ref;
      Timer        : MSSyst.Windows.Forms.Timer.Ref;
   end record;
   type My_Form is access all My_Form_Record'Class;

   function new_Form (This : My_Form := null) return My_Form;
   pragma CIL_Constructor (new_Form);
   --  Constructor for the form

end My_Form;


