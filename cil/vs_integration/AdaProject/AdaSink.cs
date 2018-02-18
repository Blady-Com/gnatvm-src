using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

namespace AdaCore.AdaPackage
{
    class AdaSink : Microsoft.VisualStudio.Package.AuthoringSink
    {
        public AdaSink(ParseReason reason, int line, int col, int maxErrors):base(reason,line,col,maxErrors) {
        }

        public bool GetCodeSpan(out TextSpan span)
        {
            span = new TextSpan();
            return false;
        }
    }
}
