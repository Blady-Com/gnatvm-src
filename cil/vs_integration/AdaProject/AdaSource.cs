using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Package;
using System.Diagnostics;
using System.Globalization;

namespace AdaCore.AdaPackage
{
    public delegate AuthoringScope ScopeCreatorCallback(ParseRequest request);

    class AdaSource : Source
    {
        private ScopeCreatorCallback scopeCreator;

        public AdaSource(LanguageService service, IVsTextLines textLines, Colorizer colorizer)
            : base(service, textLines, colorizer) {
            }
        public override TokenInfo GetTokenInfo(int line, int col)
        {
            //Trace.WriteLine("get token info" + line + ":" + col);
            TokenInfo result= base.GetTokenInfo(line, col);
            //Trace.WriteLine("token:"+result.Token+"trigger:" + result.Trigger);
            return result;
        }

        public override CommentInfo GetCommentFormat() {
            CommentInfo ci = new CommentInfo();
            ci.UseLineComments = true;
            ci.LineStart = "--";
            return ci;
        }

        public ScopeCreatorCallback ScopeCreator {
            get { return scopeCreator; }
            set { scopeCreator = value; }
        }

        private void DoFormatting(EditArray mgr, TextSpan span)
        {
            // Make sure there is one space after every comma unless followed
            // by a tab or comma is at end of line.
            IVsTextLines pBuffer = this.GetTextLines();
            if (pBuffer != null)
            {
                int numlines;
                int endCol;
                pBuffer.GetLineCount(out numlines);
                settings_pkg.set_reformat_types(false);
                pBuffer.GetLengthOfLine(span.iEndLine, out endCol);
                string s = this.GetText(span.iStartLine, 0, span.iEndLine, endCol);
                string t = reformat_pkg_pkg.reformat__2(s, span.iStartLine, span.iEndLine);

                TextSpan editTextSpan = new TextSpan();

                editTextSpan.iStartLine = span.iStartLine;
                editTextSpan.iEndLine = span.iEndLine;
                editTextSpan.iStartIndex = 0;
                editTextSpan.iEndIndex = endCol;

                // Add edit operation
                mgr.Add(new EditSpan(editTextSpan, t));
                // Apply all edits
                mgr.ApplyEdits();
            }
        }
    
        public override void ReformatSpan(EditArray mgr, TextSpan span)
        {
            string description = "Reformat code";
            CompoundAction ca = new CompoundAction(this, description);
            using (ca)
            {
                ca.FlushEditActions();      // Flush any pending edits
                DoFormatting(mgr, span);    // Format the span
            }
        }

    }
}
