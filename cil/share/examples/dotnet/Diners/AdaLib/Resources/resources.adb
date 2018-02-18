with MSSyst.Resources.ResourceManager; use MSSyst.Resources.ResourceManager;
with MSSyst.Reflection.Assembly;
with MSSyst.String; use MSSyst.String;
with Ada.Text_IO;

package body Resources is

   resManager : MSSyst.Resources.ResourceManager.Ref;
   
   function get_ResManager return MSSyst.Resources.ResourceManager.Ref is
   begin
      if resManager = null then
         resManager := MSSyst.Resources.ResourceManager.new_ResourceManager
                        (null, +("AdaLib.Properties.Resources"),
                         MSSyst.Reflection.Assembly.GetCallingAssembly);
      end if;
      return resManager;
   end get_ResManager;

   function Get_From_Name (Name : MSSyst.String.Ref) return MSSyst.Drawing.Bitmap.Ref is
   begin
      return MSSyst.Drawing.Bitmap.Ref (get_ResManager.GetObject (Name));
   end Get_From_Name;

end Resources;
