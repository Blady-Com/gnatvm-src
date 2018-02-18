--  Small example of Ada using cil2ada generated bindings to access
--  System.Windows.Forms library

with MSSyst.Windows.Forms;             use MSSyst.Windows.Forms;
with MSSyst.Windows.Forms.Application;

with My_Form; use My_Form;

procedure Hello is
begin
   The_Form := My_Form.new_Form;
   Application.Run (The_Form);
end Hello;
