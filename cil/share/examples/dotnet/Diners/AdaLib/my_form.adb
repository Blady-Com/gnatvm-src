with MSSyst.EventHandler;              use MSSyst.EventHandler;
with MSSyst.String;                    use MSSyst.String;
with MSSyst.Drawing.Color;
with MSSyst.Drawing.Image;
with MSSyst.Drawing.Point;
with MSSyst.Drawing.Size;
with MSSyst.Drawing.SizeF;
with MSSyst.Windows.Forms.Application;
with MSSyst.Windows.Forms.AutoScaleMode;
with MSSyst.Windows.Forms.Control_ControlCollection;
with MSSyst.Windows.Forms.PictureBox;
with MSSyst.Windows.Forms.PictureBoxSizeMode;

with Phil; use Phil;
with Resources;
with Room;
with Society;

package body My_Form is

   ---------------
   -- Callbacks --
   ---------------

   --  We need to use the convention CIL for those callbacks, to ensure that
   --  1- the CIL profile of those procedure will remain intact, without any
   --     extra parameter
   --  2- their 'Access attribute will have the correct format for use with
   --     delegate constructors.

   procedure On_Timeout_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class);
   pragma Convention (CIL, On_Timeout_Callback);
   --  Called on timeout

   procedure On_Exit_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class);
   pragma Convention (CIL, On_Exit_Callback);
   --  Called when 'exit' has been pressed.

   --------------
   -- new_Form --
   --------------

   function new_Form (This : My_Form := null) return My_Form
   is
      Deadobj : Form.Ref;
      pragma Unreferenced (Deadobj);
      X, Y    : Natural;
      Dead    : Integer;
      W, H    : Natural;
      dW, dH  : Natural;
      Table   : MSSyst.Windows.Forms.PictureBox.Ref;
      pragma Unreferenced (Dead);
   begin
      --  Call parent constructor
      Deadobj := Form.new_Form (Form.Ref (This));

      This.SuspendLayout;
      This.set_BackColor (MSSyst.Drawing.Color.get_White);

      --  Create subobjects
      This.Main_Menu    := MainMenu.new_MainMenu;
      This.Exit_Menu    := MenuItem.new_MenuItem;

      -- Size := This.get_Size;
      W := 89;
      dW := 240 - W;
      H := 88;
      dH := 1;

      Table := PictureBox.new_PictureBox;
      Table.set_Location (MSSyst.Drawing.Point.new_Point ((240 - 64) / 2, 2 * H + dH - 47));
      Table.set_Size (MSSyst.Drawing.Size.new_Size (64, 47));
      Table.set_SizeMode (PictureBoxSizeMode.StretchImage);
      Table.set_Image (MSSyst.Drawing.Image.Ref (Resources.Get_From_Name (+"Table")));
      Control_ControlCollection.Add
        (This.Get_Controls, Table);

      --  Philosophers
      for Id in Phil.Philosophers'Range loop
         Phil.Philosophers (Id) := new_Phil (null, Id > 3);
         Phil.Philosophers (Id).Set_Resource (Society.Name_Register (Id));

         case Id is
            when 1 =>
               X := dW / 2;
               Y := 0;
            when 2 =>
               X := 0;
               Y := dH + H;
            when 3 =>
               X := (240 - 2 * W) / 2;
               Y := dH + H + dH + H;
            when 4 =>
               X := W + (240 - 2 * W) / 2;
               Y := dH + H + dH + H;
            when 5 =>
               X := dW;
               Y := dH + H;
            when others =>
               null;
         end case;

         Phil.Philosophers (Id).set_Location
           (MSSyst.Drawing.Point.new_Point (X, Y));
      end loop;
      -- ??? Needs to be sync with room.adb
      Phil.Philosophers (1).set_Grabs (1, 2);
      Phil.Philosophers (2).set_Grabs (2, 3);
      Phil.Philosophers (3).set_Grabs (4, 3);
      Phil.Philosophers (4).set_Grabs (4, 5);
      Phil.Philosophers (5).set_Grabs (1, 5);

      --  Set the timer
      This.Timer := Timer.new_Timer;
      This.Timer.add_Tick (On_Timeout_Callback'Access);
      This.Timer.set_Interval (100); --  update screen every 0.5 sec

      --  Exit menu
      This.Exit_Menu.Set_Text (+"Quit");
      This.Exit_Menu.Add_Click (On_Exit_Callback'Access);

      --  Main form
      This.set_MinimizeBox (False);
      This.set_ControlBox (False);
      This.set_Text (+"AdaCore's Diners");
      This.set_Menu (This.Main_Menu);
      This.set_AutoScaleDimensions
        (MSSyst.Drawing.SizeF.new_SizeF (96.0, 96.0));
      This.set_AutoScaleMode (MSSyst.Windows.Forms.AutoScaleMode.Dpi);
      This.set_AutoScroll (True);
      This.set_ClientSize (MSSyst.Drawing.Size.new_Size (240, 268));

      Dead := Menu_MenuItemCollection.Add
        (This.Main_Menu.get_MenuItems, This.Exit_Menu);
      for Id in Phil.Philosophers'Range loop
         Control_ControlCollection.Add
           (This.Get_Controls, Phil.Philosophers (Id));
      end loop;
      This.Timer.set_Enabled (True);

      This.ResumeLayout (False);
      This.PerformAutoScale;

      Room.Start_Serving;

      return My_Form (This);
   end new_Form;

   -------------------------
   -- On_Timeout_Callback --
   -------------------------

   procedure On_Timeout_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class) is
   begin
      for J in Phil.Philosophers'Range loop
         Phil.Philosophers (J).Update_Actions;
      end loop;
   end On_Timeout_Callback;

   ----------------------
   -- On_Exit_Callback --
   ----------------------

   procedure On_Exit_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.EventArgs.Typ'Class) is
   begin
      Room.Abort_Diner;
      Application.Exit_K;
   end On_Exit_Callback;

end My_Form;
