using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using System.Collections;

namespace AdaCore.AdaPackage
{
   class AdaLanguageService : LanguageService
   {
      private LanguagePreferences private_preferences;
      private AdaScanner my_scanner;
      private AdaAuthoringScope my_authoring_scope;

      public AdaLanguageService()
         : base()
      {
         ada_vs_lexer_pkg.adainit();
         PopulateColorableItems();
      }

      public override Source CreateSource(IVsTextLines buffer)
      {
         return new AdaSource(this, buffer, new Colorizer(this, buffer, GetScanner(buffer)));
      }
      private ArrayList colorableItemsList;

      private void PopulateColorableItems()
      {
         if (colorableItemsList == null)
         {
            colorableItemsList = new ArrayList();
         }

         MyColorableItem dummy = new MyColorableItem("", COLORINDEX.CI_USERTEXT_FG, COLORINDEX.CI_USERTEXT_BK);

         // 0 - 5 are reserved (stock colors)
         MyColorableItem defaultText = new MyColorableItem("Text", COLORINDEX.CI_DARKBLUE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(defaultText);

         MyColorableItem defaultKeyword = new MyColorableItem("Keyword", COLORINDEX.CI_DARKBLUE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(defaultKeyword);

         MyColorableItem defaultComment = new MyColorableItem("Comment", COLORINDEX.CI_DARKBLUE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(defaultComment);

         MyColorableItem defaultIdentifier = new MyColorableItem("Identifier", COLORINDEX.CI_DARKBLUE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(defaultIdentifier);

         MyColorableItem defaultString = new MyColorableItem("String", COLORINDEX.CI_DARKBLUE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(defaultString);

         MyColorableItem defaultNumber = new MyColorableItem("Number", COLORINDEX.CI_DARKBLUE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(defaultNumber);

         MyColorableItem caps = new MyColorableItem("Ada-Keyword", COLORINDEX.CI_BLUE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(caps);

         MyColorableItem lower = new MyColorableItem("Ada-Number", COLORINDEX.CI_MAROON, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(lower);

         MyColorableItem numbers = new MyColorableItem("Ada-Comment", COLORINDEX.CI_DARKGREEN, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(numbers);

         MyColorableItem strings = new MyColorableItem("Ada-String", COLORINDEX.CI_AQUAMARINE, COLORINDEX.CI_USERTEXT_BK);
         colorableItemsList.Add(strings);

      }

      public override int GetItemCount(out int piCount)
      {
         piCount = 0;
         if (this.colorableItemsList != null)
         {
            if (this.colorableItemsList.Count > 0)
            {
               // The first color is a placeholder and is
               // never counted.
               piCount = this.colorableItemsList.Count - 1;
            }
         }
         return VSConstants.S_OK;
      }

      public override int GetColorableItem(int iIndex, out IVsColorableItem ppItem)
      {
         int retval = VSConstants.E_INVALIDARG;

         ppItem = null;

         if (this.colorableItemsList != null &&
             iIndex >= 0 && iIndex < this.colorableItemsList.Count)
         {
            //ppItem = this.colorableItemsList[iIndex] as IVsColorableItem;
            ppItem = (MyColorableItem)this.colorableItemsList[iIndex];
            retval = VSConstants.S_OK;
         }
         return retval;
      }

      public override LanguagePreferences GetLanguagePreferences()
      {
         if (private_preferences == null)
         {
            private_preferences = new LanguagePreferences(this.Site,
                typeof(AdaLanguageService).GUID,
                this.Name);
            private_preferences.Init();
         }
         return private_preferences;
      }

      public override IScanner GetScanner(IVsTextLines buffer)
      {
         if (my_scanner == null)
         {
            my_scanner = new AdaScanner();
         }
         return my_scanner;
      }

      public override string Name
      {
         get { return "Ada"; }
      }
      private bool HasDot(string line, int col)
      {
         for (int j = col; j >= 0; j--)
         {
            if (line[j] == '.')
               return true;
            if ((!char.IsLetterOrDigit(line[j])) && line[j] != '_')
               return false;
         }
         return false;
      }
      private AdaMethods remember_methods;
      private TextSpan method_start_location;
      private int param_count;
      public override AuthoringScope ParseSource(ParseRequest req)
      {
         bool match_braces = false;
         //System.Diagnostics.Trace.WriteLine("parsesource:" + req.Reason);
         if (req.Reason == ParseReason.Check ||
             req.Reason == ParseReason.None)
         {
            // Parse entire source as given in req.Text.
            // Store results in the MyAuthoringScope object.
         }
         else if (req.Reason == ParseReason.MemberSelect || req.Reason == ParseReason.CompleteWord)
         {
            // Parse the line specified in req.Line for the two
            // tokens just before req.Col to obtain the identifier
            // and the member connector symbol.
            // Examine existing parse tree for members of the identifer
            // and return a list of members in your version of the
            // Declarations class as stored in the MyAuthoringScope
            // object.
            AdaDeclarations decls = new AdaDeclarations();
            IVsTextLines lines;
            string line;

            req.View.GetBuffer(out lines);
            lines.GetLineText(req.Line, 0, req.Line + 1, 0, out line);

            if (HasDot(line, req.Col - 1))
            {
               AdaSuggestions.MakeDeclarations(req.FileName, req.Text, line, req.Col - 1, decls);
            }
            // ??? Should look for ' in order to build an attribute list.
            else
            {
               int pos2;
               lines.GetPositionOfLineIndex(req.Line, req.Col - 1, out pos2);
               AdaSuggestions.MakeReservedNames(decls);
               // ??? also add basic types (integer, string, etc)
               AdaSuggestions.MakeNames(req.Text, pos2, decls);
               System.Diagnostics.Trace.WriteLine("filename:" + req.FileName);
               System.Diagnostics.Trace.WriteLine(System.IO.Path.GetExtension(req.FileName));

               if (System.IO.Path.GetExtension(req.FileName).ToLower() == ".adb")
               {
                  string spec = System.IO.Path.ChangeExtension(req.FileName, ".ads");
                  System.Diagnostics.Trace.WriteLine(spec);

                  if (System.IO.File.Exists(spec))
                  {
                     System.Diagnostics.Trace.WriteLine("opened");
                     System.IO.StreamReader sr = new System.IO.StreamReader(spec);
                     string contents = sr.ReadToEnd();
                     AdaSuggestions.MakeNames(contents, contents.Length - 1, decls);
                  }
               }
            }
            return new AdaAuthoringScope(decls);
         }
         else if (req.Reason == ParseReason.MethodTip)
         {
            AdaMethods methods;
            // Parse the line specified in req.Line for the token
            // just before req.Col to obtain the name of the method
            // being entered.
            // Examine the existing parse tree for all method signatures
            // with the same name and return a list of those signatures
            // in your version of the Methods class as stored in the
            // MyAuthoringScope object.
            if ((req.TokenInfo.Trigger & TokenTriggers.ParameterStart) != 0)
            {
               methods = new AdaMethods();
               IVsTextLines lines;
               req.View.GetBuffer(out lines);
               string line;
               lines.GetLineText(req.Line, 0, req.Line + 1, 0, out line);
               string lineChars = line.Substring(0, req.Col - 1);
               string linesChars = req.Text;
               AdaSuggestions.MakeMethods(req.FileName, linesChars, lineChars, req.Col - 1, methods);
               // suggestions_pkg.makemethods(req.FileName, linesChars, 0, linesChars.Length - 1,
               //     lineChars, 0, lineChars.Length - 1,
               //     req.Col - 1, methods, 0);
               TextSpan ts = new TextSpan();
               ts.iStartLine = ts.iEndLine = req.Line;
               ts.iStartIndex = req.Col - 1;
               ts.iEndIndex = req.Col;
               method_start_location = ts;
               if (methods.GetCount() != 0)
               {
                  req.Sink.StartName(ts, "bob");
                  req.Sink.StartParameters(ts);
               }
               param_count = 0;
               remember_methods = methods;
            }
            else if ((req.TokenInfo.Trigger & TokenTriggers.ParameterEnd) != 0)
            {
               methods = new AdaMethods();
               req.Sink.EndParameters(method_start_location);
            }
            else
            {
               IVsTextLines lines;
               int pos, pos2;
               int quote_count = 0;
               int paren_count = 0;
               req.View.GetBuffer(out lines);
               Char[] linesChars = req.Text.ToCharArray();
               req.Sink.StartName(method_start_location, "bob");
               req.Sink.StartParameters(method_start_location);
               lines.GetPositionOfLineIndex(method_start_location.iStartLine,
                   method_start_location.iEndIndex, out pos);
               lines.GetPositionOfLineIndex(req.Line, req.Col, out pos2);
               for (int i = pos; i < pos2; i++)
               {
                  if (linesChars[i] == '(' && quote_count == 0)
                  {
                     paren_count++;
                  }
                  else if (linesChars[i] == '"')
                  {
                     quote_count = (1 - quote_count);
                  }
                  else if (linesChars[i] == ')' && quote_count == 0)
                  {
                     paren_count--;
                  }
                  else if (linesChars[i] == ',' && quote_count == 0 && paren_count == 0)
                  {
                     TextSpan ts2 = new TextSpan();
                     int line, col;
                     lines.GetLineIndexOfPosition(i, out line, out col);
                     ts2.iStartLine = line;
                     ts2.iEndLine = line;
                     ts2.iStartIndex = col - 1;
                     ts2.iEndIndex = col;
                     req.Sink.NextParameter(ts2);
                     param_count++;
                  }
               }
               methods = remember_methods;
            }

            return new AdaAuthoringScope(methods);
         }

         if (req.Sink.BraceMatching || match_braces)
         {
            TextSpan span1 = new TextSpan();
            TextSpan span2 = new TextSpan();
            int parensFound = 1;
            int charIndex;
            int piVirtualSpaces;
            int direction = vs_lexer_pkg.match_direction(req.TokenInfo.Token);
            req.View.GetNearestPosition(req.Line, req.Col, out charIndex, out piVirtualSpaces);
            if (direction == -1)
            {
               charIndex -= 2;
            }
            span1.iStartLine = req.Line;
            span1.iStartIndex = req.Col - 1;
            span1.iEndLine = req.Line;
            span1.iEndIndex = req.Col;

            if (my_authoring_scope == null)
            {
               my_authoring_scope = new AdaAuthoringScope();
            }

            if (direction == 1)
            {
               while (charIndex < req.Text.Length)
               {
                  if (req.Text[charIndex] == ')')
                  {
                     parensFound--;
                     if (parensFound == 0)
                     {
                        req.View.GetLineAndColumn(charIndex, out span2.iStartLine,
                            out span2.iStartIndex);
                        span2.iEndLine = span2.iStartLine;
                        span2.iEndIndex = span2.iStartIndex + 1;
                        req.Sink.MatchPair(span1, span2, 1);
                        return my_authoring_scope;
                     }
                  }
                  else if (req.Text[charIndex] == '(')
                  {
                     parensFound++;
                  }
                  charIndex++;
               }
            }
            else if (direction == -1)
            {
               while (charIndex >= 0)
               {
                  if (req.Text[charIndex] == '(')
                  {
                     parensFound--;
                     if (parensFound == 0)
                     {
                        req.View.GetLineAndColumn(charIndex, out span2.iStartLine,
                            out span2.iStartIndex);
                        span2.iEndLine = span2.iStartLine;
                        span2.iEndIndex = span2.iStartIndex + 1;
                        req.Sink.MatchPair(span1, span2, 1);
                        return my_authoring_scope;
                     }
                  }
                  else if (req.Text[charIndex] == ')')
                  {
                     parensFound++;
                  }
                  charIndex--;
               }
            }
         }

         return my_authoring_scope;
      }

      public override int ValidateBreakpointLocation(IVsTextBuffer buffer,
                                                     int line,
                                                     int col,
                                                     TextSpan[] pCodeSpan)
      {
         int retval = VSConstants.E_NOTIMPL;
         if (pCodeSpan != null)
         {
            // Initialize span to current line by default.
            pCodeSpan[0].iStartLine = line;
            pCodeSpan[0].iStartIndex = col;
            pCodeSpan[0].iEndLine = line;
            pCodeSpan[0].iEndIndex = col;
         }

         if (buffer != null)
         {
            IVsTextLines textLines = buffer as IVsTextLines;
            if (textLines != null)
            {
               Source src = this.GetSource(textLines);
               if (src != null)
               {
                  TokenInfo tokenInfo = new TokenInfo();
                  string text = src.GetText();
                  ParseRequest req = CreateParseRequest(src,
                                                        line,
                                                        col,
                                                        tokenInfo,
                                                        text,
                                                        src.GetFilePath(),
                                                        ParseReason.CodeSpan,
                                                        null);
                  req.Scope = this.ParseSource(req);
                  // AdaSink sink = (AdaSink)req.Sink;

                  //TextSpan span = new TextSpan();
                  // Assume line is invalid.
                  //retval = VSConstants.S_FALSE;
                  retval = VSConstants.S_OK;
                  /*if (req.Sink != null && req.Sink.GetCodeSpan(out span))
                  {
                      pCodeSpan[0] = span;
                      retval = VSConstants.S_OK;
                  }*/
               }
            }
         }
         return retval;
      }
   }

   public class MyColorableItem : ColorableItem
   {

      public MyColorableItem(string displayName, COLORINDEX foreground, COLORINDEX background)

         : base(displayName, displayName, foreground, background, System.Drawing.Color.Empty, System.Drawing.Color.Empty, FONTFLAGS.FF_DEFAULT)
      {

      }

   }

}
