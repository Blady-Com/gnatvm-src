package PhilStates is

   type States is (Breathing, Thinking, Done_Thinking,
                  Eating, Done_Eating,
                  Got_One_Stick, Got_Other_Stick, Dying);

   type Elementary_Action is record
     State    : States;
     How_Long : Natural;
   end record;

end PhilStates;
