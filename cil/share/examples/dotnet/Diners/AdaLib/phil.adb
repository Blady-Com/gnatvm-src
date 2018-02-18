--::::::::::
--phil.adb
--::::::::::
with Ada.Strings.Fixed;

with MSSyst.Drawing.Color;
with MSSyst.Drawing.ContentAlignment;
with MSSyst.Drawing.Font;
with MSSyst.Drawing.FontFamily;
with MSSyst.Drawing.FontStyle;
with MSSyst.Drawing.Image;
with MSSyst.Drawing.Point;
with MSSyst.Drawing.Size;
with MSSyst.Drawing.SystemColors;
with MSSyst.Windows.Forms.Control_ControlCollection;
with MSSyst.Windows.Forms.PictureBoxSizeMode;

with Resources;

with Society;
with Room;
with Random_Generic;
package body Phil is

  -- Dining Philosophers - Ada 95 edition
  -- Philosopher is an Ada 95 task type with discriminant.

  -- Chopsticks are assigned by a higher authority, which
  --   can vary the assignments to show different algorithms.
  -- Philosopher always grabs First_Grab, then Second_Grab.
  -- Philosopher is oblivious to outside world, but needs to
  --   communicate is life-cycle events the Maitre_D.

  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.

  subtype Think_Times is Positive range 1..8;
  package Think_Length is
    new Random_Generic (Result_Subtype => Think_Times);

  subtype Meal_Times is Positive range 1..9;
  package Meal_Length is
    new Random_Generic (Result_Subtype => Meal_Times);

  task body Philosopher is  -- My_ID is discriminant

    subtype Life_Time is Positive range 1..50;

    Who_Am_I    : Society.Unique_DNA_Codes := My_ID; -- discrim
    First_Grab  : Positive;
    Second_Grab : Positive;
    Meal_Time   : Meal_Times;
    Think_Time  : Think_Times;

  begin

     -- get assigned the first and second chopsticks here

    accept Start_Eating (Chopstick1 : in Positive;
                         Chopstick2 : in Positive) do
      First_Grab  := Chopstick1;
      Second_Grab := Chopstick2;
    end Start_Eating;

    Room.Maitre_D.Report_State (Who_Am_I, Breathing);

    for Meal in Life_Time loop

      Room.Sticks (First_Grab).Pick_Up;
      Room.Maitre_D.Report_State (Who_Am_I, Got_One_Stick, First_Grab);

      Room.Sticks (Second_Grab).Pick_Up;
      Room.Maitre_D.Report_State (Who_Am_I, Got_Other_Stick, Second_Grab);

      Meal_Time := Meal_Length.Random_Value;
      Room.Maitre_D.Report_State (Who_Am_I, Eating, Meal_Time, Meal);

      delay Duration (Meal_Time);

      Room.Maitre_D.Report_State (Who_Am_I, Done_Eating);

      Room.Sticks (First_Grab).Put_Down;
      Room.Sticks (Second_Grab).Put_Down;

      delay 1.0;

      Think_Time := Think_Length.Random_Value;
      Room.Maitre_D.Report_State (Who_Am_I, Thinking, Think_Time);
      delay Duration (Think_Time);

      Room.Maitre_D.Report_State (Who_Am_I, Done_Thinking);
    end loop;

    Room.Maitre_D.Report_State (Who_Am_I, Dying);

  end Philosopher;

   -------------------------
   -- Phil_Actions_Record --
   -------------------------

   protected body Phil_Actions_Record is

      ---------
      -- Add --
      ---------

      procedure Add (State    : in States;
                     How_Long : in Natural := 0) is
      begin
         Nb_Actions := Nb_Actions + 1;
         The_Actions (Nb_Actions) := (State, How_Long);
      end Add;

      -----------------
      -- Has_Changed --
      -----------------

      function Has_Changed return Boolean is
      begin
         return Nb_Actions > 0;
      end Has_Changed;

      ----------
      -- Read --
      ----------

      procedure Read (Actions : out Elementary_Action) is
      begin
         if Nb_Actions <= 0 then
            --  No action yet, return an empty string
            Actions := (Breathing, 0);
         else
            --  Just return the converted action string.
            Actions := The_Actions (1);
            for J in 2 .. Nb_Actions loop
              The_Actions (J - 1) := The_Actions (J);
            end loop;
            Nb_Actions := Nb_Actions - 1;
         end if;
      end Read;

   end Phil_Actions_Record;

   --------------
   -- New_Phil --
   --------------

   function New_Phil (This         : Phil_Form_Ref := null;
                      RightSeated  : Boolean) return Phil_Form_Ref
   is
      Dead   : Panel.Ref;
      pragma Unreferenced (Dead);
      Width  : constant Natural := 89;
      Height : constant Natural := 88;
      Font   : MSSyst.Drawing.Font.Ref;
   begin
      --  Call parent constructor
      Dead := Panel.new_Panel (Panel.Ref (This));

      --  Create objects
      This.Actions  := new Phil_Actions_Record;
      This.Portrait := Philpicture.new_PhilPicture;
      This.NStick1  := Label.new_Label;
      This.NStick2  := Label.new_Label;
      This.Dying    := PictureBox.new_PictureBox;

      --  Initialize the main layout
      This.set_Size
        (MSSyst.Drawing.Size.new_Size (Width, Height));

      Font := MSSyst.Drawing.Font.new_Font
        (family => MSSyst.Drawing.FontFamily.get_GenericSerif,
         emSize => 7.0,
         Style  => MSSyst.Drawing.FontStyle.Regular);

      --  Initialize the labels

      This.NStick1.set_Font (Font);
      This.NStick2.set_Font (Font);
      if not RightSeated then
         This.NStick1.set_Location (MSSyst.Drawing.Point.new_Point (78, 10));
         This.NStick2.set_Location (MSSyst.Drawing.Point.new_Point (78 , 20));
      else
         This.NStick1.set_Location
           (MSSyst.Drawing.Point.new_Point (Width - 78 - 10, 10));
         This.NStick2.set_Location
           (MSSyst.Drawing.Point.new_Point (Width - 78 - 10, 20));
      end if;
      This.NStick1.set_Size (MSSyst.Drawing.Size.new_Size (10, 10));
      This.NStick2.set_Size (MSSyst.Drawing.Size.new_Size (10, 10));
      This.NStick1.set_Visible (False);
      This.NStick2.set_Visible (False);
      Control_ControlCollection.Add
        (This.get_Controls, This.NStick1);
      Control_ControlCollection.Add
        (This.get_Controls, This.NStick2);

      --  Initialize the picture
      if not RightSeated then
         This.Portrait.set_Location (MSSyst.Drawing.Point.new_Point (0,0));
      else
         This.Portrait.set_Location
           (MSSyst.Drawing.Point.new_Point (Width - 78, 0));
      end if;
      This.Portrait.set_Name (+"portrait");
      This.Portrait.set_Size (MSSyst.Drawing.Size.new_Size (78, 88));
      This.Portrait.set_SizeMode (PictureBoxSizeMode.StretchImage);
      Control_ControlCollection.Add
        (This.Get_Controls, This.Portrait);

      This.Dying.set_Location (MSSyst.Drawing.Point.new_Point (0,0));
      This.Dying.set_Name (+"rip");
      This.Dying.set_Size (MSSyst.Drawing.Size.new_Size (89, 70));
      This.Dying.set_SizeMode (PictureBoxSizeMode.StretchImage);
      This.Dying.set_Image (MSSyst.Drawing.Image.Ref (Resources.Get_From_Name (+"Dying")));
      This.Dying.set_Visible (False);
      Control_ControlCollection.Add
        (This.Get_Controls, This.Dying);

      return Phil_Form_Ref (This);
   end New_Phil;

  ---------------
  -- Set_Grabs --
  ---------------

  procedure set_Grabs
    (Phil : access Phil_Form_Typ;
     Grab1, Grab2 : Natural) is
  begin
     Phil.NStick1.set_Text (MSSyst.String.Trim (+Natural'Image (Grab1)));
     Phil.NStick2.set_Text (MSSyst.String.Trim (+Natural'Image (Grab2)));
  end set_Grabs;

   ------------------
   -- Set_Resource --
   ------------------

   procedure Set_Resource
     (Phil : access Phil_Form_Typ;
      Name : MSSyst.String.Ref) is
   begin
      Phil.Portrait.set_Image
        (MSSyst.Drawing.Image.Ref
          (Resources.Get_From_Name (Name)));
   end Set_Resource;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Phil     : access Phil_Form_Typ;
      State    : in States;
      How_Long : in Natural := 0)
   is
   begin
      Phil.Actions.Add (State, How_Long);
   end Add_Action;

   --------------------
   -- Update_Actions --
   --------------------

   procedure Update_Actions (Phil : access Phil_Form_Typ)
   is
      Action : Elementary_Action;
   begin
      while Phil.Actions.Has_Changed loop
         Phil.Actions.Read (Action);
         Phil.Portrait.Set_State (Action);

         case Action.State is

            when Breathing =>
               Phil.NStick1.set_Visible (True);
               Phil.NStick1.set_ForeColor (MSSyst.Drawing.Color.get_Red);

            when Thinking =>
               null;

            when Done_Thinking =>
               Phil.NStick1.set_Visible (True);
               Phil.NStick1.set_ForeColor (MSSyst.Drawing.Color.get_Red);

            when Eating =>
               null;

            when Done_Eating =>
               Phil.NStick1.set_Visible (False);
               Phil.NStick2.set_Visible (False);

            when Got_One_Stick =>
               Phil.NStick1.set_ForeColor (MSSyst.Drawing.Color.get_Green);
               Phil.NStick2.set_Visible (True);
               Phil.NStick2.set_ForeColor (MSSyst.Drawing.Color.get_Red);

            when Got_Other_Stick =>
               Phil.NStick2.set_ForeColor (MSSyst.Drawing.Color.get_Green);

            when Dying =>
               Phil.Portrait.set_Visible (False);
               Phil.Dying.set_Visible (True);

         end case; -- State
      end loop;
   end Update_Actions;

end Phil;
