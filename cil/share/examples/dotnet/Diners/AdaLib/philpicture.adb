with MSSyst.Windows.Forms.PaintEventArgs;
with MSSyst.Windows.Forms.PaintEventHandler;
use MSSyst.Windows.Forms.PaintEventHandler;
with MSSyst.Windows.Forms.ContainerControl;
with MSSyst.Drawing.Color;
with MSSyst.Drawing.FontFamily;
with MSSyst.Drawing.FontStyle;
with MSSyst.Drawing.Graphics;
with MSSyst.Drawing.GraphicsUnit;
with MSSyst.Drawing.Imaging.ImageAttributes;
with MSSyst.Drawing.Rectangle;
with MSSyst.Drawing.RectangleF;
with MSSyst.Drawing.Size;
with MSSyst.Drawing.SizeF;
with MSSyst.Drawing.SolidBrush;
with MSSyst.Object;
with MSSyst.String; use MSSyst.String;

with Resources;

package body Philpicture is

   procedure On_Paint_Legend_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.Windows.Forms.PaintEventArgs.Typ'Class);
   pragma Convention (CIL, On_Paint_Legend_Callback);

   procedure On_Paint_Icon_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.Windows.Forms.PaintEventArgs.Typ'Class);
   pragma Convention (CIL, On_Paint_Icon_Callback);

   ---------------------
   -- new_PhilPicture --
   ---------------------

   function new_PhilPicture (This : Phil_Picture := null) return Phil_Picture
   is
      DeadObj : MSSyst.Windows.Forms.PictureBox.Ref;
      pragma Unreferenced (DeadObj);
   begin
      --  Call parent constructor
      DeadObj := MSSyst.Windows.Forms.PictureBox.new_PictureBox (MSSyst.Windows.Forms.PictureBox.Ref (This));

      This.Font := MSSyst.Drawing.Font.new_Font
        (null,
         family => MSSyst.Drawing.FontFamily.get_GenericSerif,
         emSize => 7.0,
         Style  => MSSyst.Drawing.FontStyle.Regular);
      This.Think := MSSyst.Drawing.Image.Ref
        (Resources.Get_From_Name (+"Thinking"));
      This.Meal1 := MSSyst.Drawing.Image.Ref
        (Resources.Get_From_Name (+"Meal1"));
      This.Meal2 := MSSyst.Drawing.Image.Ref
        (Resources.Get_From_Name (+"Meal2"));
      This.Meal3 := MSSyst.Drawing.Image.Ref
        (Resources.Get_From_Name (+"Meal3"));
      This.add_Paint (On_Paint_Legend_Callback'Access);
      This.add_Paint (On_Paint_Icon_Callback'Access);

      return This;
   end new_PhilPicture;

   --------------
   -- Set_Text --
   --------------

   procedure Set_State
     (Picture : access Phil_Picture_Record'Class;
      State   : PhilStates.Elementary_Action) is
   begin
      Picture.State    := State.State;
      Picture.How_Long := State.How_Long;
      Picture.Refresh;
   end Set_State;

   ----------------------------
   -- On_Paint_Icon_Callback --
   ----------------------------

   procedure On_Paint_Icon_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.Windows.Forms.PaintEventArgs.Typ'Class)
   is
      This      : constant Phil_Picture := Phil_Picture (Sender);
      Graphics  : constant MSSyst.Drawing.Graphics.Ref :=
                    MSSyst.Drawing.Graphics.Ref (E.get_Graphics);
      DstRect   : MSSyst.Drawing.Rectangle.ValueType;
      CliSize   : MSSyst.Drawing.Size.ValueType;
      Image     : MSSyst.Drawing.Image.Ref;
      ImageAttr : MSSyst.Drawing.Imaging.ImageAttributes.Ref;
      Ratio     : Integer;
      UpRight   : Boolean := False;

   begin
      case This.State is
         when Breathing =>
            Image := This.Meal1;
         when Thinking =>
            Image := This.Think;
            UpRight := True;
         when Done_Thinking =>
            Image := This.Meal1;
         when Got_One_Stick =>
            Image := This.Meal2;
         when Got_Other_Stick | Eating =>
            Image := This.Meal3;
         when Done_Eating =>
            return;
         when Dying =>
            return;
      end case;

      Ratio := Integer ((96.0 * 2.0) / Graphics.get_DpiX);

      CliSize := This.get_ClientSize;

      if UpRight then
         DstRect := MSSyst.Drawing.Rectangle.new_Rectangle
           (x      => CliSize.get_Width - Image.get_Width / Ratio,
            y      => 0,
            width  => Image.get_Width / Ratio,
            height => Image.get_Height / Ratio);
      else
         DstRect := MSSyst.Drawing.Rectangle.new_Rectangle
           (x      => 0,
            y      => CliSize.get_Height - (Image.get_Height / Ratio),
            width  => Image.get_Width / Ratio,
            height => Image.get_Height / Ratio);
      end if;

      ImageAttr := MSSyst.Drawing.Imaging.ImageAttributes.new_ImageAttributes;
      ImageAttr.SetColorKey
        (MSSyst.Drawing.Color.get_Red, MSSyst.Drawing.Color.get_Red);

      Graphics.DrawImage
        (Image,
         DstRect,
         0, 0, Image.get_Width, Image.get_Height,
         MSSyst.Drawing.GraphicsUnit.Pixel, ImageAttr);
   end On_Paint_Icon_Callback;

   ------------------------------
   -- On_Paint_Legend_Callback --
   ------------------------------

   procedure On_Paint_Legend_Callback
     (Sender : access MSSyst.Object.Typ'Class;
      E      : access MSSyst.Windows.Forms.PaintEventArgs.Typ'Class)
   is
      This     : constant Phil_Picture := Phil_Picture (Sender);
      Graphics : constant MSSyst.Drawing.Graphics.Ref :=
                   MSSyst.Drawing.Graphics.Ref (E.get_Graphics);
      Rect     : MSSyst.Drawing.RectangleF.ValueType;
      StrSize  : MSSyst.Drawing.SizeF.ValueType;
      CliSize  : MSSyst.Drawing.Size.ValueType;
      Center   : Boolean := False;

   begin
      case This.State is
         when Breathing =>
            This.Text := +"hungry !";
         when Thinking =>
            Center := True;
            This.Text := MSSyst.String.Ref
              (MSSyst.String.Concat
                (str0 => +"Thinking (",
                 str1 => MSSyst.String.Trim (+Natural'Image (This.How_Long)),
                 str2 => +"s)"));
         when Done_Thinking =>
            This.Text := +"hungry !";

         when Got_One_Stick =>
            This.Text := +"HUNGRY !!!";

         when Got_Other_Stick =>
            null;

         when Eating =>
            This.Text := MSSyst.String.Ref
              (MSSyst.String.Concat
                (str0 => +"Miam ! (",
                 str1 => MSSyst.String.Trim (+Natural'Image (This.How_Long)),
                 str2 => +"s)"));
         when Done_Eating =>
            Center := True;
            This.Text := +"""BURP !""";

         when Dying =>
            Center := True;
            This.Text := +"Croak !";
      end case;

      StrSize := Graphics.MeasureString
        (text => This.Text,
         font => This.Font);
      CliSize := This.get_ClientSize;

      if Center then
         Rect := MSSyst.Drawing.RectangleF.new_RectangleF
           (X => (Float (CliSize.get_Width) - StrSize.get_Width) / 2.0,
            Y => Float (CliSize.get_Height) - StrSize.get_Height,
            width => StrSize.get_Width,
            height => StrSize.get_Height);
      else
         Rect := MSSyst.Drawing.RectangleF.new_RectangleF
           (X => Float (CliSize.get_Width) - StrSize.get_Width,
            Y => Float (CliSize.get_Height) - StrSize.get_Height,
            width => StrSize.get_Width,
            height => StrSize.get_Height);
      end if;

      Graphics.DrawString
        (s               => This.Text,
         font            => This.Font,
         brush           => MSSyst.Drawing.SolidBrush.new_SolidBrush
           (color =>  MSSyst.Drawing.Color.get_Black),
         layoutRectangle => Rect);
   end On_Paint_Legend_Callback;

end Philpicture;
