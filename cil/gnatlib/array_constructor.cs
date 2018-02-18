//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                    a r r a y _ c o n s t r u c t o r                     //
//                                                                          //
//                     Copyright (C) 2003-2011, AdaCore                     //
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
// This work is partially  based on A#, an Ada  compiler for .NET by  Prof. //
// Martin C. Carlisle of the United States Air Force Academy.               //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

// Create a constructor for multi-dimensional arrays of objects

using System;

namespace mgnat.adalib {

public sealed class array_constructor  {
   private static string type_name;
   private static System.Reflection.Assembly caller;

   ///////////////////
   // Set_Type_Name //
   ///////////////////

   public static void set_type_name (string the_type_name)
   {
      caller = System.Reflection.Assembly.GetCallingAssembly();

      if (the_type_name.StartsWith ("[")) {
         int idx = the_type_name.IndexOf(']');
         type_name = the_type_name.Remove (0, idx + 1);
      } else {
         type_name = the_type_name;
         caller = System.Reflection.Assembly.GetCallingAssembly();
      }
   }

   ////////////////
   // Dimensions //
   ////////////////

	private static string dimensions (int num_dimensions)
	{
		string s = "";

		for (int i=0; i<num_dimensions; i++) {
			s = s + "[]";
		}

		return s;
	}

   ////////////////
   // Make_Array //
   ////////////////

	public static Array make_array (params int[] x)
	{
		Array       result;
		int         num_dimensions = x.Length;
      System.Type t1             = System.Type.GetType
                                    (type_name + dimensions(num_dimensions-1));
               
      if (t1 == null) {
         t1 = caller.GetType (type_name + dimensions (num_dimensions-1));
      }

      result = System.Array.CreateInstance (t1, x[0]);

      if (num_dimensions > 1) {
		   int[] y = new int[num_dimensions - 1];

         for (int j = 1; j < num_dimensions; j++) {
             y [j - 1] = x [j];
         }

         for (int i = 0; i < x[0]; i++) {
             result.SetValue (make_array (y), i);
         }
      }

		return result;
	}

	public static Array make_array (int x)
   {
      int[] args = new int[1];
      args[0] = x;

      return make_array(args);
   }

	public static Array make_array (int x, int y)
   {
      int[] args = new int[2];
      args[0] = x;
      args[1] = y;

      return make_array(args);
   }

	public static Array make_array (int x, int y, int z)
   {
      int[] args = new int[3];
      args[0] = x;
      args[1] = y;
      args[2] = z;

      return make_array(args);
   }

	public static Array make_array (int x, int y, int z, int w)
   {
      int[] args = new int[4];
      args[0] = x;
      args[1] = y;
      args[2] = z;
      args[3] = w;

      return make_array(args);
   }

	public static Array make_array (int x, int y, int z, int w, int v)
   {
      int[] args = new int[5];
      args[0] = x;
      args[1] = y;
      args[2] = z;
      args[3] = w;
      args[4] = v;

      return make_array(args);
   }

	public static Array make_array (int x, int y, int z, int w, int v, int u)
   {
      int[] args = new int[6];
      args[0] = x;
      args[1] = y;
      args[2] = z;
      args[3] = w;
      args[4] = v;
      args[5] = u;

      return make_array(args);
   }

	public static Array make_array (int x, int y, int z, int w, int v, int u,
                                   int t)
   {
      int[] args = new int[7];
      args[0] = x;
      args[1] = y;
      args[2] = z;
      args[3] = w;
      args[4] = v;
      args[5] = u;
      args[6] = t;

      return make_array(args);
   }

} // array_constructor

} // mgnat.adalib
