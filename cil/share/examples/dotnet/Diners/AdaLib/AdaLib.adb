with MSSyst.Windows.Forms;             use MSSyst.Windows.Forms;
with MSSyst.Windows.Forms.Application;

with My_Form; use My_Form;

procedure AdaLib is
begin
   --  Just run the application using my_form as main form.
   Application.Run(My_Form.new_Form);
end AdaLib;
