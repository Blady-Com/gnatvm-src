package AdaLib is

   type Cb_Type is access procedure (I : Integer);

   procedure A_Callback (I : Integer);
   procedure Another_Callback (I : Integer);
   
   procedure Execute (Cb : Cb_Type);

end AdaLib;
