--::::::::::
--room.adb
--::::::::::
with Chop;
with Phil;
with Society;
with Calendar;
pragma Elaborate (Phil);
package body Room is
 
  -- Dining Philosophers, Ada 95 edition
  -- A line-oriented version of the Room package
  -- Michael B. Feldman, The George Washington University, 
  -- July, 1995.

  -- philosophers sign into dining room, giving Maitre_D their DNA code
 
  Dijkstra  : aliased Phil.Philosopher (My_ID => 1);
  Stroustrup: aliased Phil.Philosopher (My_ID => 2);
  Anderson  : aliased Phil.Philosopher (My_ID => 3);
  Ichbiah   : aliased Phil.Philosopher (My_ID => 4);
  Taft      : aliased Phil.Philosopher (My_ID => 5);
 
  type Philosopher_Ptr is access all Phil.Philosopher;

  Phils : array (Table_Type) of Philosopher_Ptr;
  Phil_Seats : array (Society.Unique_DNA_Codes) of Table_Type;

  procedure Start_Serving is
  begin
     Maitre_D.Start_Serving;
  end Start_Serving;

  procedure Abort_Diner is
  begin
     abort Maitre_D;

     for J in Phils'Range loop
        abort Phils (J).all;
     end loop;     
  end Abort_Diner;

  task body Maitre_D is
 
    T          : Natural;
    Start_Time : Calendar.Time;
    Blanks : constant String := "     ";

  begin
 
    accept Start_Serving;

    Start_Time := Calendar.Clock;
 
    -- now Maitre_D assigns phils to seats at the table

    Phils :=
      (Dijkstra'Access,
       Stroustrup'Access,
       Anderson'Access,
       Ichbiah'Access,
       Taft'Access);
  
    Phil_Seats   := (1, 2, 3, 4, 5);   -- which seat each phil occupies

    -- and assigns them their chopsticks.

    Phils (1).Start_Eating (1, 2);
    Phils (2).Start_Eating (2, 3);
    Phils (3).Start_Eating (4, 3);
    Phils (4).Start_Eating (4, 5);
    Phils (5).Start_Eating (1, 5);
 
    loop
      select
        accept Report_State (Which_Phil : in Society.Unique_DNA_Codes;
                             State      : in PhilStates.States;
                             How_Long   : in Natural := 0;
                             Which_Meal : in Natural := 0) do

          T := Natural (Calendar."-" (Calendar.Clock, Start_Time));
          Phil.Add_Action 
            (Phil.Philosophers (Phil_Seats (Which_Phil)),
             State, How_Long);
          
        end Report_State;
 
      or
        terminate;
      end select;
 
    end loop;
 
  end Maitre_D;
 
end Room;
