with Ada.Text_IO;

package body AdaLib is

   I : Integer := 0;

   procedure A_Callback (I : Integer) is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (I) & ": Inside Ada callback");
   end A_Callback;

   procedure Another_Callback (I : Integer) is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (I) & ": Inside another Ada callback");
   end Another_Callback;

   procedure Execute (Cb : Cb_Type) is
   begin
      Ada.Text_IO.Put_Line ("Ada method calling a callback:");
      I := I + 1;
      Cb (I);
   end Execute;
   
end AdaLib;
