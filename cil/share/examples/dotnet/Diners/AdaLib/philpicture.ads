with MSSyst.Drawing.Font;
with MSSyst.Drawing.Image;
with MSSyst.String;                   use MSSyst.String;
with MSSyst.Windows.Forms.PictureBox;
with PhilStates;                      use PhilStates;

package Philpicture is

   type Phil_Picture_Record is new
     MSSyst.Windows.Forms.PictureBox.Typ
   with record
      Font     : MSSyst.Drawing.Font.Ref;
      Text     : MSSyst.String.Ref;
      Think    : MSSyst.Drawing.Image.Ref;
      Meal1    : MSSyst.Drawing.Image.Ref;
      Meal2    : MSSyst.Drawing.Image.Ref;
      Meal3    : MSSyst.Drawing.Image.Ref;
      State    : PhilStates.States;
      How_Long : Natural;
   end record;
   type Phil_Picture is access all Phil_Picture_Record'Class;

   function new_PhilPicture (This : Phil_Picture := null) return Phil_Picture;
   pragma CIL_Constructor (new_PhilPicture);

   procedure Set_State
     (Picture : access Phil_Picture_Record'Class;
      State   : PhilStates.Elementary_Action);

end Philpicture;
