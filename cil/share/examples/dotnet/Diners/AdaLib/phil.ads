--::::::::::
--phil.ads
--::::::::::
with Ada.Calendar;                     use Ada.Calendar;

with MSSyst.String;                    use MSSyst.String;
with MSSyst.Windows.Forms;             use MSSyst.Windows.Forms;
with MSSyst.Windows.Forms.Label;
with MSSyst.Windows.Forms.Panel;
with MSSyst.Windows.Forms.TextBox;
with MSSyst.Windows.Forms.PictureBox;
with Philpicture;
with PhilStates;  use PhilStates;
with Society; use Society;
package Phil is

  -- Dining Philosophers - Ada 95 edition
  -- Philosopher is an Ada 95 task type with discriminant
  -- Michael B. Feldman, The George Washington University,
  -- July, 1995.

  task type Philosopher (My_ID : Society.Unique_DNA_Codes) is
    entry Start_Eating (Chopstick1 : in Positive;
                        Chopstick2 : in Positive);
  end Philosopher;

  type Actions is array (1 .. 50) of Elementary_Action;

  protected type Phil_Actions_Record is

     procedure Add (State    : in States;
                    How_Long : in Natural := 0);
     --  Add a new action performed by the philosopher

     function Has_Changed return Boolean;
     --  Tell if the actions list has changed since last read

     procedure Read (Actions : out Elementary_Action);
     --  Read the current actions

  private
     The_Actions : Actions := (others => (Dying, 0));
     Nb_Actions  : Natural := 0;
  end Phil_Actions_Record;

  type Phil_Actions is access all Phil_Actions_Record;

  type Phil_Form_Typ is new Panel.Typ with record
     Actions  : Phil_Actions;
     Portrait : Philpicture.Phil_Picture;
     NStick1  : Label.Ref;
     NStick2  : Label.Ref;
     Dying    : MSSyst.Windows.Forms.PictureBox.Ref;
  end record;
  type Phil_Form_Ref is access all Phil_Form_Typ'Class;

  function New_Phil (This         : Phil_Form_Ref := null;
                     RightSeated  : Boolean) return Phil_Form_Ref;
  pragma CIL_Constructor (New_Phil);
   --  CIL constructor for the phil panel.

  procedure set_Grabs
    (Phil : access Phil_Form_Typ;
     Grab1, Grab2 : Natural);

  procedure Set_Resource
    (Phil : access Phil_Form_Typ;
     Name : MSSyst.String.Ref);
  --  Set the name of the philosopher
  --  Only the task that created the phil panel is allowed to call this
  --  procedure.

  procedure Add_Action
    (Phil     : access Phil_Form_Typ;
     State    : in States;
     How_Long : in Natural := 0);
  --  Add an action performed by the philosopher
  --  This procedure can be called by any task.

  procedure Update_Actions (Phil : access Phil_Form_Typ);
  --  Make the philosopher panel update the actions.
  --  Only the task that created the phil panel is allowed to call this
  --  procedure.

  Philosophers : array (Unique_DNA_Codes) of Phil_Form_Ref;
  --  List of philosophers.

end Phil;
