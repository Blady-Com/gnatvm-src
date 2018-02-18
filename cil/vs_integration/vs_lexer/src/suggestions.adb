
with VS_Lexer;

package body Suggestions is

   ---------------
   -- MakeNames --
   ---------------

   procedure MakeNames(Buffer : in Wide_String;
         Declarations : access Adadeclarations.Typ'Class) is
      Current_Loc : Integer := 0;
      Token : Vs_Lexer.Token_Record;
      Previous_Type : Vs_Lexer.Tokens := Vs_Lexer.Dot_T;
   begin
      for I in Vs_Lexer.Reservedwords loop
         declare
            X : String := To_Lower(Vs_Lexer.Reservedwords'Image(I));
         begin
            Declarations.AddDeclaration(x(x'first..x'last-2));
         end;
      end loop;

      while Current_Loc < Buffer'Last loop
         Token := Vs_Lexer.Get_Token
           (X              => Buffer,
            Current_Offset => Current_Loc);

         if Token.Kind = Vs_Lexer.Name_T
           and then Previous_Type /= VS_Lexer.Dot_T
         then
            declare
               bob : String :=
                       Ada.Characters.Conversions.To_String
                         (Buffer (Token.Start_Index .. Token.End_Index));
            begin
               Declarations.AddDeclaration(Bob);
            end;
         end if;

         Previous_Type := Token.Kind;
         Current_Loc := Token.End_Index + 1;
      end loop;
   end MakeNames;

end Suggestions;
