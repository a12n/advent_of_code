package body Advent is
   procedure Generic_Swap (A, B : in out Value_Type) is
      Temp : constant Value_Type := A;
   begin
      A := B;
      B := Temp;
   end Generic_Swap;
end Advent;
