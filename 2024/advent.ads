package Advent is
   pragma Pure;

   generic
      type Value_Type is private;
   procedure Generic_Swap (A, B : in out Value_Type);
end Advent;
