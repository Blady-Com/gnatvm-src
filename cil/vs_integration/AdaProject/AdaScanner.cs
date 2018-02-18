using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Package;

namespace AdaCore.AdaPackage
{

    class AdaScanner : IScanner
    {
        private char[] the_string;
        private int current_offset;
        private string temp_string;
        #region IScanner Members

        bool IScanner.ScanTokenAndProvideInfoAboutIt(TokenInfo tokenInfo, ref int state)
        {
            vs_lexer.token_record result;
            if (current_offset >= the_string.Length)
            {
                return false;
            }
            result = vs_lexer_pkg.get_token(the_string, 0, the_string.Length - 1, current_offset);
            state = 0;
            tokenInfo.StartIndex = result.start_index;
            tokenInfo.Token = result.kind;
            tokenInfo.Trigger = (TokenTriggers) result.triggers;
            tokenInfo.Type = (TokenType) result.class_of;
            tokenInfo.EndIndex = result.end_index;
            //System.Diagnostics.Trace.WriteLine(temp_string.Substring(result.start_index,
            //    result.end_index - result.start_index + 1));
            //System.Diagnostics.Trace.WriteLine(tokenInfo.Token.ToString()+":"+tokenInfo.Type.ToString());
            current_offset = result.end_index + 1;
            switch (tokenInfo.Type)  {
                case TokenType.Comment:
                case TokenType.LineComment:
                    //tokenInfo.Color = TokenColor.Comment;
                    tokenInfo.Color = ((TokenColor)8);
                    break;
                case TokenType.Keyword:
                    //tokenInfo.Color = TokenColor.Keyword;
                    tokenInfo.Color = ((TokenColor)6);
                    break;
                case TokenType.String:
                    //tokenInfo.Color = TokenColor.String;
                    tokenInfo.Color = ((TokenColor) 9);  // was 9
                    break;
                case TokenType.Identifier:
                    //tokenInfo.Color = TokenColor.Identifier;
                    tokenInfo.Color = TokenColor.Text;
                    break; 
                case TokenType.Literal:
                    tokenInfo.Color = ((TokenColor) 7);
                    break;
                case TokenType.Delimiter:
                case TokenType.Operator:
                case TokenType.Text:
                case TokenType.Unknown:
                case TokenType.WhiteSpace:
                    tokenInfo.Color = TokenColor.Text;
                    break;
            }
            return true;
        }

        void IScanner.SetSource(string source, int offset)
        {
            the_string = source.ToCharArray(offset,source.Length-offset);
            temp_string = source.Substring(offset, source.Length - offset);
            current_offset = 0;
        }

        #endregion
    }
}
