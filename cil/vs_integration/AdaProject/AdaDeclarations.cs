using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Package;

namespace AdaCore.AdaPackage
{
    public class AdaDeclarations : Declarations
    {
        IList<string> declarations;
        private bool sorted = false;

        public AdaDeclarations()
        {
            this.declarations = new System.Collections.Generic.List<string>();
        }
        public AdaDeclarations(IList<string> declarations)
        {
            this.declarations = declarations;
            ((System.Collections.Generic.List<string>)this.declarations).Sort();
            int i = 0;
            while (i < this.declarations.Count - 2)
            {
                if (this.declarations[i].ToLower() == this.declarations[i + 1].ToLower())
                    this.declarations.RemoveAt(i + 1);
                else
                    i++;
            }
            this.sorted = true;
        }

        // allow me to build this 1 at a time
        public void addDeclaration(string declaration)
        {
            this.declarations.Add(declaration);
        }

        private string Get(int index)
        {
            if (!this.sorted)
            {
                ((System.Collections.Generic.List<string>)this.declarations).Sort();
                int i = 0;
                while (i < this.declarations.Count - 2)
                {
                    if (this.declarations[i].ToLower() == this.declarations[i + 1].ToLower())
                        this.declarations.RemoveAt(i + 1);
                    else
                        i++;
                } 
                this.sorted = true;
            }
            return index >= 0 && index < declarations.Count ? declarations[index] : "";
        }

        public override int GetCount()
        {
            return declarations.Count;
        }

        public override string GetDisplayText(int index)
        {
            return Get(index);
        }

        public override string GetName(int index)
        {
            return Get(index);
        }

        public override string GetDescription(int index)
        {
            return Get(index);
        }

        public override int GetGlyph(int index)
        {
            return 0;
        }
    }
}
