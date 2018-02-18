//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                          O b j e c t _ F i l e                           //
//                                                                          //
//                     Copyright (C) 1998-2006, AdaCore                     //
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

//  This C# file is part of the GNAT for .NET library and is used to implement
//  support for the Ada.Direct_IO and Ada.Sequential_IO generic packages.
//  It implements files that support reading and writing of arbitrary
//  objects using Java Object streams, and allows random access to the
//  object elements. This class is from called GNAT_libc, as well as directly
//  from Ada.Sequential_IO and Ada.Direct_IO (for Read, Write, and certain
//  index-related operations).

using System.IO;
using System;
namespace mgnat.adalib {


public class Object_File {

  public String filename;
  //  the name of the object file

  public System.IO.FileStream file;
  //  The Java File object associated with the corresponding Ada file

#if !COMPACT
  public System.Runtime.Serialization.Formatters.Binary.BinaryFormatter formatter
     = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter();
#endif

  //  Input and output streams associated with the Ada file

  public long index = 0;
  //  The current index for reading or writing objects to the file

  public bool create;
  //  If create is set to true in the object file constructor, then
  //  this is a new file any existing file of the same name will be
  //  be replaced. Otherwise this file will reading or updating an
  //  an existing file.

  public bool update;
  //  If update is set to true in the object file constructor, then the
  //  object file allows updating of existing objects within the file.

  public bool updated = false;
  //  This per-file field is set to true if any updates occur to
  //  the file, however we don't currently test this value to
  //  determine whether the file needs to be written out (leads
  //  to problems with ACVC tests that attempt file deletion ???).

  public System.Collections.Generic.List<object> file_elements;
  //  A vector of objects associated with the file, which is used
  //  for managing reads and updates to file elements. In the case
  //  of opening an existing file, the vector is filled by reading
  //  all objects from the file when it is opened. On closing the
  //  file, all of the object elements in the vector are written
  //  out to the file. This is potentially very space-consumptive
  //  in the case of large files, because the entire contents of
  //  files needs to reside in memory, but this is necessary to
  //  support Direct_IO files, since Java doesn't provide a
  //  convenient mechanism for support random I/O on objects
  //  (note that in general the flattened representation of
  //  objects in object streams results in variable-sized
  //  records). We should be able to reduce this overhead of
  //  keeping the whole file in memory for Sequential_IO files,
  //  by providing a way to communicate on the opening of the
  //  file whether access is sequential or random, but currently
  //  we can't tell (because the fopen call does not communicate
  //  enough information). One way to work around this without
  //  changing the fopen interface would be to add an additional
  //  operation which could be called by Ada.Sequential_IO and
  //  Ada.Direct_IO immediately following the normal fopen call. ???

  //  This is the file constructor, which will initialize the
  //  file_elements Vector and load it from the contents of
  //  the file in the case of opening an existing file.

  public Object_File (String filename, bool create, bool update)
  {
#if !COMPACT
     this.filename = filename;
     this.create = create;
     this.update = update;

     if (create) {

        if (File.Exists (filename)) File.Delete (filename);

        //  If a new file needs to be created, attempt the creation
        //  here and raise Name_Error if it can't be created (we
        //  do this here just to catch invalid file names; the real
        //  output file will be created in the close() operation).
        //  But perhaps this routine should create the output file
        //  for real rather than leaving it for the close(). ???

        try {
           this.file = File.Create(filename);
           this.file.Close ();
           this.file_elements = new System.Collections.Generic.List<object> (100);
        }
        catch (System.Exception) {
           throw new ada.io_exceptions.name_error ();
        }
     }

     //  This is an attempted Open of an existing file

     else {
        if (!File.Exists (filename)) {
           throw new System.IO.FileNotFoundException ();
        }
        else {
           this.file = File.Open (filename, FileMode.Open);

           this.file_elements = new System.Collections.Generic.List<object> (100);

           try {
              while (true) {
                 this.file_elements.Add (this.formatter.Deserialize (this.file));
              }
           }
           catch (System.Exception) {
              //  No action in the case of reaching end of file
           }

           // Finished reading contents of the file, so we can close it now

           this.file.Close ();
        }
     }

     this.index = 1;
#else
     throw new System.Exception();
#endif
  }

  //  Closes the file of objects, writing out the sequence of objects
  //  contained in the file_elements Vector in the case of an output
  //  file.
  public void close () {
     this.Close ();
  }

  public void Close () {
#if !COMPACT
     if (this.create || this.update) {
        this.file
          = File.Create (this.filename);

        // Loop through the vector, writing out each vector element...
        int count;
        for (count = 0; count < this.file_elements.Count; count++) {
           this.formatter.Serialize (this.file,this.file_elements[count]);
        }

        this.file.Flush ();
        this.file.Close ();
     }

     this.index = 0;
     this.file_elements = null;
#else
        throw new System.Exception();
#endif
  }

  //  Returns true (end-of-file condition) when the file index is equal to
  //  the current size of the file_elements Vector.

  static public bool object_eof (Object file) {
     Object_File obj_file = (Object_File) file;

     return obj_file.index > (long) obj_file.file_elements.Count;
  }

  //  Sets the current file index according to the parameter 'index'

  static public void set_file_index (Object file, long index) {
     ((Object_File) file).index = index;
  }

  //  Returns the current index of the file (index for reading or
  //  writing objects to the file_elements Vector).

  static public long file_index (Object file) {
     return ((Object_File) file).index;
  }

  //  Returns the number of object elements in the file (i.e.,
  //  the size of the file_elements Vector).

  static public long file_size (Object file) {
     return (long) ((Object_File) file).file_elements.Count;
  }

  //  Reads the next element from the file (the object in the file_elements
  //  Vector at the current file index - 1). The exception End_Error will
  //  be thrown if no element exists at the currently specified index.

  static public Object read_element (Object file) {
     Object_File obj_file = (Object_File) file;

     try {
        return obj_file.file_elements[(int) obj_file.index - 1];
     }
     catch (ArgumentOutOfRangeException) {
        throw new ada.io_exceptions.end_error ();
     }
  }

  //  Writes an object (and all of its associated component objects if any)
  //  to the file at the current index (appending or updating the element
  //  at the corresponding index in the file_elements Vector). In the case
  //  where the index is set past the end of the current size of file_elements,
  //  the intervening Vector elements, if any, will also be filled with the
  //  written object.

  static public void write_element (Object file, Object element) {
     Object_File obj_file = (Object_File) file;

     long extension_count
           = obj_file.index - (long) obj_file.file_elements.Count;

     //  Append the element to the end of the file, adding as many additional
     //  element copies as needed when extension_count > 1.

     if (extension_count > 0) {

        long elt_count;

        for (elt_count = 1; elt_count <= extension_count; elt_count++) {
           obj_file.file_elements.Add (element);
        }
     }

     //  The write is an update to an existing element, so remove the
     //  element at that index and replace it with the new element.

     else {
        obj_file.file_elements.RemoveAt ((int) obj_file.index - 1);
        obj_file.file_elements.Insert
                                      ((int) obj_file.index - 1, element);

     }

     obj_file.updated = true;
  }

}
} //namespace

