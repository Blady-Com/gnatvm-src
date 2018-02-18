//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                          O b j e c t _ F i l e                           //
//                                                                          //
//                     Copyright (C) 2000-2005 AdaCore                      //
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
// JGNAT - The GNAT Ada 95 toolchain for the Java Virtual Machine is        //
//         maintained by Ada Core Technologies, Inc. - http://www.gnat.com  //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

//  This Java class is part of the JGNAT library and provides low-level
//  support for the Ada.Direct_IO and Ada.Sequential_IO generic packages.
//  It implements files that support reading and writing of arbitrary
//  objects using Java Object streams, and allows random access to the
//  object elements. This class is from called GNAT_libc, as well as directly
//  from Ada.Sequential_IO and Ada.Direct_IO (for Read, Write, and certain
//  index-related operations).

package jgnat.adalib;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Vector;

public class Object_File {

  public File file;
  //  The Java File object associated with the corresponding Ada file

  public ObjectInputStream  input_file;
  public ObjectOutputStream output_file;
  //  Input and output streams associated with the Ada file

  public long index = 0;
  //  The current index for reading or writing objects to the file

  public boolean create;
  //  If create is set to true in the object file constructor, then
  //  this is a new file any existing file of the same name will be
  //  be replaced. Otherwise this file will reading or updating an
  //  an existing file.

  public boolean update;
  //  If update is set to true in the object file constructor, then the
  //  object file allows updating of existing objects within the file.

  public boolean updated = false;
  //  This per-file field is set to true if any updates occur to
  //  the file, however we don't currently test this value to
  //  determine whether the file needs to be written out (leads
  //  to problems with ACVC tests that attempt file deletion ???).

  public Vector file_elements;
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

  public Object_File (File file, boolean create, boolean update)
    throws java.io.IOException, java.io.StreamCorruptedException,
           java.lang.ClassNotFoundException
  {
     this.file = file;
     this.create = create;
     this.update = update;

     if (create) {

        if (file.exists ()) {
           file.delete ();

           this.file_elements = new Vector (100, 100);
        }
        else {
           //  If a new file needs to be created, attempt the creation
           //  here and raise Name_Error if it can't be created (we
           //  do this here just to catch invalid file names; the real
           //  output file will be created in the close() operation).
           //  But perhaps this routine should create the output file
           //  for real rather than leaving it for the close(). ???

           try {
              this.output_file
                = new ObjectOutputStream (new FileOutputStream (file));
              this.output_file.close ();
              this.file_elements = new Vector (100, 100);
           }
           catch (java.io.IOException e) {
              throw new ada$io_exceptions$name_error ();
           }
        }
     }

     //  This is an attempted Open of an existing file

     else {
        if (!file.exists ()) {
           throw new java.io.FileNotFoundException ();
           // this.file_elements = new Vector (100, 100);
        }
        else {
           this.input_file = new ObjectInputStream (new FileInputStream (file));

           this.file_elements = new Vector (100, 100);

           try {
              while (true) {
                 this.file_elements.addElement (this.input_file.readObject ());
              }
           }
           catch (java.io.EOFException e) {
              //  No action in the case of reaching end of file
           }

           // Finished reading contents of the file, so we can close it now

           this.input_file.close ();
        }
     }

     this.index = 1;
  }

  //  Closes the file of objects, writing out the sequence of objects
  //  contained in the file_elements Vector in the case of an output
  //  file.

  public void close () throws java.io.IOException {

     if (this.create || this.update) {
        this.output_file
          = new ObjectOutputStream (new FileOutputStream (this.file));

        // Loop through the vector, writing out each vector element...
        int count;
        for (count = 0; count < this.file_elements.size (); count++) {
           this.output_file.writeObject (this.file_elements.elementAt (count));
        }

        this.output_file.flush ();
        this.output_file.close ();
     }

     this.index = 0;
     this.file_elements = null;
  }

  //  Returns true (end-of-file condition) when the file index is equal to
  //  the current size of the file_elements Vector.

  static public boolean object_eof (Object file) {
     Object_File obj_file = (Object_File) file;

     return obj_file.index > (long) obj_file.file_elements.size ();
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
     return (long) ((Object_File) file).file_elements.size ();
  }

  //  Reads the next element from the file (the object in the file_elements
  //  Vector at the current file index - 1). The exception End_Error will
  //  be thrown if no element exists at the currently specified index.

  static public Object read_element (Object file) {
     Object_File obj_file = (Object_File) file;

     try {
        return obj_file.file_elements.elementAt ((int) obj_file.index - 1);
     }
     catch (ArrayIndexOutOfBoundsException e) {
        throw new ada$io_exceptions$end_error ();
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
           = obj_file.index - (long) obj_file.file_elements.size ();

     //  Append the element to the end of the file, adding as many additional
     //  element copies as needed when extension_count > 1.

     if (extension_count > 0) {

        long elt_count;

        for (elt_count = 1; elt_count <= extension_count; elt_count++) {
           obj_file.file_elements.addElement (element);
        }
     }

     //  The write is an update to an existing element, so remove the
     //  element at that index and replace it with the new element.

     else {
        obj_file.file_elements.removeElementAt ((int) obj_file.index - 1);
        obj_file.file_elements.insertElementAt
                                      (element, (int) obj_file.index - 1);

     }

     obj_file.updated = true;
  }

}

