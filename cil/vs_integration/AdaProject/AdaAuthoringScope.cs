using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Package;

namespace AdaCore.AdaPackage
{
   class AdaAuthoringScope : AuthoringScope
   {
      private Declarations myDecls;
      private Methods myMethods;

      public AdaAuthoringScope()
      {
      }
      public AdaAuthoringScope(Declarations decls)
      {
         myDecls = decls;
      }
      public AdaAuthoringScope(Methods methods)
      {
         myMethods = methods;
      }

      public override string GetDataTipText(int line, int col, out Microsoft.VisualStudio.TextManager.Interop.TextSpan span)
      {
         span = new Microsoft.VisualStudio.TextManager.Interop.TextSpan();
         // can set a span here where the tool tip would be displayed
         /*span.iStartLine = 0;
         span.iStartIndex = 1;
         span.iEndLine = 0;
         span.iEndIndex = 4;*/
         return "";
      }

      public override Declarations GetDeclarations(Microsoft.VisualStudio.TextManager.Interop.IVsTextView view, int line, int col, TokenInfo info, ParseReason reason)
      {
         return myDecls;
      }

      public override Methods GetMethods(int line, int col, string name)
      {
         //System.Diagnostics.Trace.WriteLine("getting methods from ada authoring scope");
         return myMethods;
      }

      public override string Goto(Microsoft.VisualStudio.VSConstants.VSStd97CmdID cmd, Microsoft.VisualStudio.TextManager.Interop.IVsTextView textView, int line, int col, out Microsoft.VisualStudio.TextManager.Interop.TextSpan span)
      {
         span = new Microsoft.VisualStudio.TextManager.Interop.TextSpan();
         return "";
      }
   }
}
