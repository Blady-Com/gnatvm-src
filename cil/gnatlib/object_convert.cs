//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       o b j e c t _ c o n v e r t                        //
//                                                                          //
//                       Copyright (C) 2006, AdaCore                        //
//                                                                          //
// GNAT is free software;  you can  redistribute it  and/or modify it under //
// terms of the  GNU General Public License as published  by the Free Soft- //
// ware  Foundation;  either version 2,  or (at your option) any later ver- //
// sion.  GNAT is distributed in the hope that it will be useful, but WITH- //
// OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY //
// or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License //
// for  more details.  You should have  received  a copy of the GNU General //
// Public License  distributed with GNAT;  see file COPYING.  If not, write //
// to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, //
// MA 02111-1307, USA.                                                      //
//                                                                          //
// The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by //
// AdaCore - http://www.adacore.com                                         //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

//  Class used to implement unchecked_conversions between different
//  classes having the same fields/layout

using System;
using System.Reflection;

namespace mgnat.adalib {

public sealed class object_convert
{
    public static Object class_conversion
      (Object src, RuntimeTypeHandle destType)
    {
        if (src == null)
          return src;

        {
          FieldInfo[] srcFieldInfo;
          FieldInfo[] destFieldInfo;
          Type dest_type = Type.GetTypeFromHandle (destType);
          Type src_type = src.GetType();

          if (dest_type == src_type || dest_type.IsAbstract)
            return src;

          // Get the type and fields of src.
          srcFieldInfo = src_type.GetFields
            (BindingFlags.NonPublic
             | BindingFlags.Instance
             | BindingFlags.Public);
          destFieldInfo = dest_type.GetFields
            (BindingFlags.NonPublic
             | BindingFlags.Instance
             | BindingFlags.Public);

          if (srcFieldInfo.Length != destFieldInfo.Length)
            return src;

          try
          {
              Object dest = Activator.CreateInstance (dest_type);
              for(int i = 0; i < srcFieldInfo.Length; i++)
                destFieldInfo[i].SetValue
                  (dest, srcFieldInfo[i].GetValue (src));
              return dest;
          }
          catch (Exception)
          {
              return src;
          }
        }
    }
}
}
