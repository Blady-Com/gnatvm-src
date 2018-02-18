using System;
using System.Collections.Generic;
using System.Text;

namespace AdaCore.AdaPackage
{
   class AdaSuggestions
   {
      private static AdaDeclarations Remember_Declarations;
      private static string lastInserted;
      private static AdaMethods rememberMethods;
      private static string packageName;
      private static string procedureName;
      private static string displayProcedureName;

      private static string Dashify(string Str)
      {
         char[] res = Str.ToCharArray();
         for (int j = 0; j < res.Length; j++)
         {
            if (res[j] == '.') res[j] = '-';
         }
         return new String(res);
      }

      private static string Dottify(string Str)
      {
         char[] res = Str.ToCharArray();
         for (int j = 0; j < res.Length; j++)
         {
            if (res[j] == '-') res[j] = '.';
         }
         return new String(res);
      }

      private static bool IsBasicName(string Name, AdaDeclarations Declarations)
      {
         if (Name.ToLower() == "ada")
         {
            Declarations.addDeclaration("Calendar");
            Declarations.addDeclaration("Characters");
            Declarations.addDeclaration("Command_Line");
            Declarations.addDeclaration("Complex_Text_IO");
            Declarations.addDeclaration("Containers");
            Declarations.addDeclaration("Decimal");
            Declarations.addDeclaration("Direct_IO");
            Declarations.addDeclaration("Directories");
            Declarations.addDeclaration("Environment_Variables");
            Declarations.addDeclaration("Exceptions");
            Declarations.addDeclaration("Finalization");
            Declarations.addDeclaration("Float_Text_IO");
            Declarations.addDeclaration("Float_Wide_Text_IO");
            Declarations.addDeclaration("Integer_Text_IO");
            Declarations.addDeclaration("Integer_Wide_Text_IO");
            Declarations.addDeclaration("Long_Float_Text_IO");
            Declarations.addDeclaration("Long_Float_Wide_Text_IO");
            Declarations.addDeclaration("Real_Time");
            Declarations.addDeclaration("Sequential_IO");
            Declarations.addDeclaration("Strings");
            Declarations.addDeclaration("Tags");
            Declarations.addDeclaration("Text_IO");
            Declarations.addDeclaration("Unchecked_Conversion");
            Declarations.addDeclaration("Unchecked_Deallocation");
            Declarations.addDeclaration("Wide_Text_IO");
            Declarations.addDeclaration("Wide_Characters");
            return true;
         }
         else if (Name.ToLower() == "ada.characters")
         {
            Declarations.addDeclaration("Conversions");
            Declarations.addDeclaration("Handling");
            Declarations.addDeclaration("Latin_1");
            Declarations.addDeclaration("Wide_Latin_1");
            return true;
         }
         else if (Name.ToLower() == "ada.containers")
         {
            Declarations.addDeclaration("Doubly_Linked_Lists");
            Declarations.addDeclaration("Hashed_Maps");
            Declarations.addDeclaration("Hashed_Sets");
            Declarations.addDeclaration("Hash_Tables");
            Declarations.addDeclaration("Indefinite_Doubly_Linked_Lists");
            Declarations.addDeclaration("Indefinite_Hashed_Maps");
            Declarations.addDeclaration("Indefinite_Hashed_Sets");
            Declarations.addDeclaration("Indefinite_Ordered_Maps");
            Declarations.addDeclaration("Indefinite_Ordered_Multisets");
            Declarations.addDeclaration("Indefinite_Ordered_Sets");
            Declarations.addDeclaration("Ordered_Maps");
            Declarations.addDeclaration("Ordered_Multisets");
            Declarations.addDeclaration("Ordered_Sets");
            Declarations.addDeclaration("Red_Black_Trees");
            Declarations.addDeclaration("Prime_Numbers");
            Declarations.addDeclaration("Vectors");
            return true;
         }
         else if (Name.ToLower() == "ada.numerics")
         {
            Declarations.addDeclaration("Complex_Elementary_Functions");
            Declarations.addDeclaration("Complex_Types");
            Declarations.addDeclaration("Discrete_Random");
            Declarations.addDeclaration("Elementary_Functions");
            Declarations.addDeclaration("Float_Random");
            Declarations.addDeclaration("Long_Complex_Elementary_Functions");
            Declarations.addDeclaration("Long_Complex_Types");
            Declarations.addDeclaration("Long_Elementary_Functions");
            return true;
         }
         else if (Name.ToLower() == "ada.strings")
         {
            Declarations.addDeclaration("Bounded");
            Declarations.addDeclaration("Fixed");
            Declarations.addDeclaration("Maps");
            Declarations.addDeclaration("Superbounded");
            Declarations.addDeclaration("Unbounded");
            Declarations.addDeclaration("Wide_Bounded");
            Declarations.addDeclaration("Wide_Fixed");
            Declarations.addDeclaration("Wide_Maps");
            Declarations.addDeclaration("Wide_Superbounded");
            Declarations.addDeclaration("Wide_Unbsounded");
            return true;
         }
         else if (Name.ToLower() == "ada.strings.unbounded")
         {
            Declarations.addDeclaration("Text_IO");
            Declarations.addDeclaration("Unbounded_String");
            return true;
         }
         else if (Name.ToLower() == "ada.text_io")
         {
            Declarations.addDeclaration("Create");
            Declarations.addDeclaration("Enumeration_IO");
            Declarations.addDeclaration("Get");
            Declarations.addDeclaration("Get_Line");
            Declarations.addDeclaration("Open");
            Declarations.addDeclaration("Put");
            Declarations.addDeclaration("Put_Line");
            Declarations.addDeclaration("Skip_Line");
            return true;
         }
         else if (Name.ToLower() == "ada.wide_characters")
         {
            Declarations.addDeclaration("Unicode");
            return true;
         }
         return false;
      }

      private static string Krunch(string Filename)
      {
         byte[] exportedFilename = new byte[Filename.Length];
         mgnat.adalib.Int Len = new mgnat.adalib.Int();
         Len.all = exportedFilename.Length;
         for (int j = 0; j < Filename.Length; j++) exportedFilename[j] = (byte)Filename[j];
         krunch.krunch(exportedFilename, 1, exportedFilename.Length, Len, 8, false, false);
         char[] result = new char[Len.all];
         for (int j = 0; j < Len.all; j++)
            result[j] = (char)exportedFilename[j];
         return new String(result);
      }

      private static void Lb_Insert (mcc.gnat_tools.xref.entity the_entity)
      {
         char[] Entity_Name = new char[the_entity.name_length];
         for (int j = 0; j < Entity_Name.Length; j++)
            Entity_Name[j] = (char)the_entity.name[j];

         if (new String(Entity_Name) != lastInserted)
         {
            lastInserted = new String(Entity_Name);
            Remember_Declarations.addDeclaration(lastInserted);
         }
      }

      private static string TokenName(int Token_Type)
      {
         int index = vs_lexer_pkg.tokensN[Token_Type] - 1;
         int length = vs_lexer_pkg.tokensN[Token_Type + 1] - index - 1;
         char[] ret = new char[length];
         for (int j = 0; j < length; j++)
            ret[j] = (char)vs_lexer_pkg.tokensS[j + index];
         return new String(ret);
      }

      public static void MakeReservedNames
         (AdaDeclarations Declarations)
      {
         for (int j = 0; j < vs_lexer_pkg.tokensN.Length; j++)
         {
            string tName = TokenName(j).ToLower();
            Declarations.addDeclaration(tName.Substring(0, tName.Length - 2));
            if (tName == "xor_t") break; // xor_t is the last reserved keyword
         }
      }

      public static void MakeNames
          (string Buffer, int Buffer_Last, AdaDeclarations Declarations)
      {
         int CurrentLoc = 0;
         vs_lexer.token_record Token;
         string Previous_Type = "DOT_T";

         while (CurrentLoc < Buffer_Last)
         {
            Token = vs_lexer_pkg.get_token(Buffer.ToCharArray(), 0, Buffer_Last, CurrentLoc);
            if ((TokenName(Token.kind) == "NAME_T") && (Previous_Type != "DOT_T"))
            {
               string bob = Buffer.Substring(Token.start_index, Token.end_index - Token.start_index + 1);
               Declarations.addDeclaration(bob);
            }
            Previous_Type = TokenName(Token.kind);
            CurrentLoc = Token.end_index + 1;
         }
      }


      public static void MakeDeclarations
          (string TheFileName,
           string Buffer,
           string Line,
           int Column,
           AdaDeclarations Declarations)
      {
         int Start = 0;
         int End = Column;
         string FileName = TheFileName.ToLower();

         for (int j = Column; j >= 0; j--)
         {
            if (!Char.IsLetterOrDigit(Line[j]) && Line[j] != '.' && Line[j] != '_')
            {
               Start = j + 1;
               break;
            }
         }
         for (int j = Column; j > Start; j--)
         {
            if (Line[j] == '.')
            {
               End = j - 1;
               break;
            }
         }

         // maybe we've got something like 3 .
         if (Start > End) return;

         string Prefix;
         string FoundFilename;
         string FoundDirectory;
         char[] tmp = new char[End - Start + 1];
         for (int j = Start; j <= End; j++) tmp[j - Start] = (char)Line[j];
         Prefix = new String(tmp).ToLower();
         mcc.gnat_tools.xref.xref_file_list_package.node Xrefs = new mcc.gnat_tools.xref.xref_file_list_package.node();

         if (IsBasicName(Prefix, Declarations)) return;

         {
            string tmpFilename = Dashify(Prefix);
            string FullPath = DefaultLocation.Find_File(tmpFilename, "", System.IO.Directory.GetParent(FileName).FullName, true);
            if (FullPath.Length == 0)
            {
               tmpFilename = Krunch(tmpFilename);
               FullPath = DefaultLocation.Find_File(tmpFilename, "", System.IO.Directory.GetParent(FileName).FullName, true);
            }
            if (FullPath.Length == 0) return;

            FoundFilename = tmpFilename + ".ads";
            FoundDirectory = DefaultLocation.Find_File(tmpFilename, "", System.IO.Directory.GetParent(FileName).FullName, false);
            FoundDirectory = System.IO.Directory.GetParent(FoundDirectory).FullName;

            mgnat.adalib.Acc Xrefs_Acc = new mgnat.adalib.Acc();
            byte[] bFileName = new byte[FullPath.Length];
            for (int j = 0; j < FullPath.Length; j++)
               bFileName[j] = (byte)FullPath[j];
            Xrefs_Acc.all = null;

            mcc.gnat_tools.xref_pkg.read_ali(bFileName, 0, bFileName.GetUpperBound(0), Xrefs_Acc);

            if (Xrefs_Acc.all != null)
            {
               bFileName = new byte[FoundFilename.Length];
               for (int j = 0; j < FoundFilename.Length; j++)
                  bFileName[j] = (byte)FoundFilename[j];
               Xrefs = (mcc.gnat_tools.xref.xref_file_list_package.node)Xrefs_Acc.all;
               lastInserted = "";
               Remember_Declarations = Declarations;

               mcc.gnat_tools.xref_pkg.enumerate_public_entities
                   (bFileName, 0, bFileName.Length - 1, Xrefs, new mcc.gnat_tools.xref.public_callback_type (Lb_Insert));
            }
         }
         if (FoundDirectory.Length != 0)
         {
            string[] results = System.IO.Directory.GetFiles(FoundDirectory, Dashify(Prefix) + "-" + "*.ad*");
            for (int j = 0; j < results.Length; j++)
            {
               string thisOne = System.IO.Path.GetFileNameWithoutExtension(results[j]);
               string answer = thisOne.Substring(Prefix.Length, thisOne.Length - Prefix.Length);
               if (answer.IndexOf('-') == -1)
                  Declarations.addDeclaration(answer);
            }
         }
      }

      private static string GetUseClause(string buffer)
      {
         vs_lexer.token_record Token;
         int currentLoc = 0;
         string useClauses = "";

         while (currentLoc < buffer.Length)
         {
            Token = vs_lexer_pkg.get_token(buffer.ToCharArray(), 0, buffer.Length - 1, currentLoc);
            currentLoc = Token.end_index + 1;
            if (TokenName(Token.kind) == "USE_T")
            {
               //  Skip the first token which must be a space
               Token = vs_lexer_pkg.get_token(buffer.ToCharArray(), 0, buffer.Length - 1, currentLoc);
               currentLoc = Token.end_index + 1;
               while ((TokenName(Token.kind) != "SEMICOLON_T") || (TokenName(Token.kind) != "COMMA_T"))
               {
                  Token = vs_lexer_pkg.get_token(buffer.ToCharArray(), 0, buffer.Length - 1, currentLoc);
                  currentLoc = Token.end_index + 1;
                  useClauses = useClauses + buffer.Substring(Token.start_index, Token.end_index - Token.start_index + 1);
               }
               useClauses = useClauses + ";";
            }
            else if ((TokenName(Token.kind) == "PROCEDURE_T") ||
                       (TokenName(Token.kind) == "PACKAGE_T") ||
                       (TokenName(Token.kind) == "FUNCTION_T"))
            {
               break;
            }
         }
         return useClauses;
      }

      private static void Lb_Parameter_Insert(mcc.gnat_tools.xref.entity the_entity)
      {
         mcc.gnat_tools.xref.entity_list_package.node walk;
         mcc.gnat_tools.xref.entity param;
         int paramIndex;
         char[] Entity_Name = new char[the_entity.name_length];
         for (int j = 0; j < Entity_Name.Length; j++) Entity_Name[j] = (char)the_entity.name[j];

         if (new String(Entity_Name).ToLower() == procedureName)
         {
            if (the_entity.type_length > 0)
            {
               char[] Type_Name = new char[the_entity.type_length];
               for (int j = 0; j < Type_Name.Length; j++) Type_Name[j] = (char)the_entity.typ[j];
               rememberMethods.addFunction(displayProcedureName, new String(Type_Name));
            }
            else
            {
               rememberMethods.addProcedure(displayProcedureName);
            }
            paramIndex = 0;
            walk = the_entity.parameters;
            while (walk != null)
            {
               param = mcc.gnat_tools.xref_pkg.entity_list_package_car(walk);
               char[] paramName = new char[param.name_length];
               char[] paramType = new char[param.type_length];
               string paramMode = "";
               for (int j = 0; j < paramName.Length; j++) paramName[j] = (char)param.name[j];
               for (int j = 0; j < paramType.Length; j++) paramType[j] = (char)param.typ[j];
               if ((char)the_entity.param_modes[paramIndex] == '>')
                  paramMode = "in ";
               else if ((char)the_entity.param_modes[paramIndex] == '<')
                  paramMode = "out ";
               else if ((char)the_entity.param_modes[paramIndex] == '=')
                  paramMode = "in out ";
               else if ((char)the_entity.param_modes[paramIndex] == '^')
                  paramMode = "access ";
               rememberMethods.addParameter(new String(paramName) + " : " + paramMode + new String(paramType));
               walk = mcc.gnat_tools.xref_pkg.entity_list_package_cdr(walk);
               paramIndex++;
            }
         }
      }

      public static void MakeMethods
          (string FileName,
           string Buffer,
           string Line,
           int Column,
           AdaMethods Methods)
      {
         string adaFileName = FileName.ToLower();
         int nameFirst = 0;
         int nameLast = Line.Length - 1;
         int noDotNameFirst = 0;
         bool haveOne = false;
         mcc.gnat_tools.xref.xref_file_list_package.node xrefs = null;
         int useLocation = 0;
         int newUseLocation = 0;
         string thisUse;
         string strUseClause = GetUseClause(Buffer);
         string possibleDot;

         rememberMethods = Methods;
         for (int j = nameLast; j >= nameFirst; j--)
         {
            if (Char.IsLetterOrDigit(Line[j]) || Line[j] == '.' || Line[j] == '_')
            {
               if (!haveOne)
               {
                  haveOne = true;
                  nameLast = j;
               }
               if (Line[j] == '.')
               {
                  noDotNameFirst = noDotNameFirst > j + 1 ? noDotNameFirst : j + 1;
               }
            }
            else if (Char.IsControl(Line[j]) || Line[j] == ' ')
            {
               if (haveOne)
               {
                  nameFirst = j + 1;
                  noDotNameFirst = noDotNameFirst > nameFirst ? noDotNameFirst : nameFirst;
                  break;
               }
            }
            else
            {
               nameFirst = j + 1;
               noDotNameFirst = noDotNameFirst > nameFirst ? noDotNameFirst : nameFirst;
            }
         }
         if (!haveOne) return;

         displayProcedureName = Line.Substring(noDotNameFirst, nameLast - noDotNameFirst + 1);
         procedureName = displayProcedureName.ToLower();
         packageName = Line.Substring(nameFirst, noDotNameFirst - 2 - nameFirst + 1).ToLower();

         while (newUseLocation != -1)
         {
            newUseLocation = strUseClause.IndexOf(';', useLocation);
            if (newUseLocation != -1)
            {
               thisUse = strUseClause.Substring(useLocation, newUseLocation - 1 - useLocation + 1);
               useLocation = newUseLocation + 1;
               if ((thisUse.Length == 0) && (packageName.Length == 0))
               {
                  goto EndMainLoop;
               }
            }
            else
               thisUse = "";
            if ((thisUse.Length != 0) && (packageName.Length != 0))
               possibleDot = ".";
            else
               possibleDot = "";

            {
               string filename = Dashify(thisUse + possibleDot + packageName).ToLower();
               string fullPath = DefaultLocation.Find_File(filename, "", System.IO.Directory.GetParent(adaFileName).FullName, true);
               string foundFilename;
               if (fullPath.Length == 0)
               {
                  filename = Krunch(filename);
                  fullPath = DefaultLocation.Find_File(filename, "", System.IO.Directory.GetParent(adaFileName).FullName, true);
                  if (fullPath.Length == 0)
                     goto EndMainLoop;
               }
               foundFilename = filename + ".ads";
               byte[] fullPathByte = new byte[fullPath.Length];
               char[] tmp = fullPath.ToCharArray();
               mgnat.adalib.Acc xrefs_ptr = new mgnat.adalib.Acc();
               for (int j = 0; j < tmp.Length; j++) fullPathByte[j] = (byte)tmp[j];
               mcc.gnat_tools.xref_pkg.read_ali(fullPathByte, 0, fullPath.Length - 1, xrefs_ptr);
               if (xrefs_ptr.all != null)
               {
                  xrefs = (mcc.gnat_tools.xref.xref_file_list_package.node)xrefs_ptr.all;
                  lastInserted = "";
                  byte[] foundFilenameByte = new byte[foundFilename.Length];
                  for (int j = 0; j < foundFilename.Length; j++) foundFilenameByte[j] = (byte)foundFilename[j];
                  mcc.gnat_tools.xref_pkg.enumerate_public_entities
                      (foundFilenameByte, 1, foundFilename.Length, xrefs, new mcc.gnat_tools.xref.public_callback_type (Lb_Parameter_Insert));
                  xrefs = null;
               }
            }

         EndMainLoop:
            xrefs = null;
         }
      }
   }
}
