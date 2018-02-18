using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Package;

namespace AdaCore.AdaPackage
{
   public class ParameterInfo
   {
      private string myDescription, myName, myDisplay;
      public ParameterInfo(string name, string display, string description)
      {
         myDescription = description;
         myName = name;
         myDisplay = display;
      }
      public string description
      {
         get
         {
            return myDescription;
         }
      }
      public string display
      {
         get
         {
            return myDisplay;
         }
      }
      public string name
      {
         get
         {
            return myName;
         }
      }
   }

   public class FunctionInfo
   {
      private string myDescription, myType, myName;
      IList<ParameterInfo> myParameters;

      public FunctionInfo(string name, string description, string type, IList<ParameterInfo> parameters)
      {
         myDescription = description;
         myType = type;
         myParameters = parameters;
         myName = name;
      }
      public void addParameter(string name)
      {
         myParameters.Add(new ParameterInfo(name, name, ""));
      }

      public void GetParameterInfo(int parameter, out string name, out string display, out string description)
      {
         if (myParameters != null && 0 <= parameter && parameter < myParameters.Count)
         {
            name = myParameters[parameter].name;
            display = myParameters[parameter].display;
            description = myParameters[parameter].description;
         }
         else
         {
            name = display = description = string.Empty;
         }
      }

      public string Description
      {
         get
         {
            return myDescription;
         }
      }
      public string Type
      {
         get
         {
            return myType;
         }
      }
      public string Name
      {
         get
         {
            return myName;
         }
      }
      public int ParameterCount
      {
         get
         {
            return myParameters.Count;
         }
      }
   }

   public class AdaMethods : Methods
   {
      private IList<FunctionInfo> methods;
      private FunctionInfo current;
      private int currentIndex;
      private bool HasParameters()
      {
         if (this.methods != null && 0 <= currentIndex && this.methods.Count >= currentIndex)
         {
            if (this.methods[currentIndex].ParameterCount != 0)
            {
               return true;
            }
            else
            {
               return false;
            }
         }
         else
         {
            return true;
         }
      }

      public AdaMethods()
      {
         this.currentIndex = -1;
         this.methods = new System.Collections.Generic.List<FunctionInfo>();
      }

      public AdaMethods(IList<FunctionInfo> methods)
      {
         this.currentIndex = -1;
         this.methods = methods;
      }
      public override string Delimiter
      {
         get
         {
            return "; ";
         }
      }
      public override string OpenBracket
      {
         get
         {
            if (this.HasParameters())
               return "(";
            else
               return "";
         }
      }
      public override string CloseBracket
      {
         get
         {
            if (this.HasParameters())
               return ")";
            else
               return "";
         }
      }
      public override bool TypePrefixed
      {
         get
         {
            return false;
         }
      }
      // allow me to build this 1 at a time
      public void addProcedure(string name)
      {
         current = new FunctionInfo("procedure " + name, "", "", new System.Collections.Generic.List<ParameterInfo>());
         this.methods.Add(current);
      }
      public void addFunction(string name, string ret)
      {
         current = new FunctionInfo("function " + name, "", "return " + ret, new System.Collections.Generic.List<ParameterInfo>());
         this.methods.Add(current);
      }
      public void addParameter(string name)
      {
         current.addParameter(name);
      }
      public override int GetCount()
      {
         return methods != null ? methods.Count : 0;
      }

      public override string GetDescription(int index)
      {
         this.currentIndex = index;
         return methods != null && 0 <= index && index < methods.Count ? methods[index].Description : "";
      }

      public override string GetType(int index)
      {
         this.currentIndex = index;
         return methods != null && 0 <= index && index < methods.Count ? methods[index].Type : "";
      }

      public override int GetParameterCount(int index)
      {
         this.currentIndex = index;
         return methods != null && 0 <= index && index < methods.Count ? methods[index].ParameterCount : 0;
      }

      public override void GetParameterInfo(int index, int parameter, out string name, out string display, out string description)
      {
         this.currentIndex = index;
         if (methods != null && 0 <= index && index < methods.Count)
         {
            methods[index].GetParameterInfo(parameter, out name, out display, out description);
         }
         else
         {
            name = display = description = string.Empty;
         }
      }

      public override string GetName(int index)
      {
         this.currentIndex = index;
         return methods != null && 0 <= index && index < methods.Count ? methods[index].Name : "";
      }
   }

}
