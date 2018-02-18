with Ada.Strings.Fixed;

with MSSyst.EventHandler;              use MSSyst.EventHandler;
with MSSyst.Drawing.Color;
with MSSyst.Drawing.ContentAlignment;
with MSSyst.Drawing.Point;
with MSSyst.Drawing.Size;
with MSSyst.Drawing.SystemColors;
with MSSyst.Windows.Forms.Application;
with MSSyst.Windows.Forms.Control_ControlCollection;
with MSSyst.Windows.Forms.Menu_MenuItemCollection;

package body My_Form is

   procedure On_Action_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class);
   pragma Convention (CIL, On_Action_Callback);

   procedure On_Exit_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class);
   pragma Convention (CIL, On_Exit_Callback);

   ---------------
   -- MAIN FORM --
   ---------------

   function new_Form
     (This : My_Form := null) return My_Form
   is
      DeadObj : Form.Ref;
      pragma Unreferenced (DeadObj);
   begin
      --  Call parent
      DeadObj := Form.new_Form (Form.Ref (This));

      This.Initialize_Components;
      return This;
   end new_Form;

   procedure Initialize_Components (This : access My_Form_Record) is
      Dead    : Integer;
      pragma Unreferenced (Dead);
   begin
      This.SuspendLayout;

      --  Create subobjects
      This.Color        := MSSyst.Drawing.Color.get_Indigo;
      This.Main_Menu    := MainMenu.new_MainMenu;
      This.Action_Menu  := MenuItem.new_MenuItem;
      This.Exit_Menu    := MenuItem.new_MenuItem;
      This.Some_Text    := Label.new_Label;

      --  Main menu
      Dead := Menu_MenuItemCollection.Add
        (This.Main_Menu.get_MenuItems, This.Action_Menu);
      Dead := Menu_MenuItemCollection.Add
        (This.Main_Menu.get_MenuItems, This.Exit_Menu);

      --  Action menu
      This.Action_Menu.set_Text (+"Click Me !");
      This.Action_Menu.Add_Click (On_Action_Callback'Access);

      --  Label
      This.Some_Text.set_Location
        (MSSyst.Drawing.Point.new_Point (0, 0));
      This.Some_Text.set_Size
        (Form.get_ClientSize (Form.Ref (This)));
      This.Some_Text.set_TextAlign
        (MSSyst.Drawing.ContentAlignment.TopCenter);
      Control_ControlCollection.Add
        (This.Get_Controls, This.Some_Text);

      --  Exit menu
      This.Exit_Menu.set_Text (+"Quit");
      This.Exit_Menu.Add_Click (On_Exit_Callback'Access);

      --  This form
      This.set_Text (+"Hello from the Ada World !");
      This.set_Menu (This.Main_Menu);

      This.ResumeLayout (False);
   end Initialize_Components;

   procedure On_Action_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class)
   is
      Obj : MenuItem.Ref := MenuItem.Ref (Sender);
      use MSSyst.Windows.Forms.Label;
   begin
      if +Obj.get_Text = "Toggled !" then
         Obj.set_Text (+"Click Me !");
         The_Form.Some_Text.set_Text (+"");
      else
         Obj.set_Text (+"Toggled !");
         The_Form.Some_Text.set_ForeColor (The_Form.Color);
         The_Form.Some_Text.set_Text (+"Welcome to the Ada for .NET Compact Framework World !");
      end if;
   end On_Action_Callback;

   procedure On_Exit_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class) is
   begin
      Application.Exit_K;
   end On_Exit_Callback;

end My_Form;
