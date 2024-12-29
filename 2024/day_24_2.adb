with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_24; use Advent.Day_24;
with Advent;        use Advent;

procedure Day_24_2 is
   --  45 x input wires
   --  45 y input wires
   --  46 z output wires
   --  176 gates
   Wires : Wire_Map := Get_Wires (Standard_Input);

   procedure Unswap (A, B : Wire_Name) is
      use Wire_Maps;

      I : constant Cursor := Wires.Find (A);
      J : constant Cursor := Wires.Find (B);

      A_Wire : constant Wire_Type := I.Element;
      B_Wire : constant Wire_Type := J.Element;
   begin
      Wires.Replace_Element (I, B_Wire);
      Wires.Replace_Element (J, A_Wire);
   end Unswap;

   --  The solution, mainly found with analyzing circuit visualization
   --  and trying to validate adders with Day_24.Full_Adder and
   --  Day_24.Half_Adder functions.
   --
   --  Start validating full-adders from the last one. On invalid
   --  adder analyze circuit visualization (see day_24-graphviz),
   --  analyze the adder with invalid output. Add an entry here for
   --  the swapped components.
   Swapped : Wire_Name_Array (1 .. 8) :=
     [
   --  Pair 1
   "z35", "cfk",
   --  Pair 2
   "z18", "dmn",
   --  Pair 3
   "qjj", "cbj",
   --  Pair 4
   "z07", "gmt"];
begin
   --  Restore swapped pair outputs.
   for I in 1 .. 4 loop
      Unswap (Swapped (2 * I - 1), Swapped (2 * I));
   end loop;

   --  Validate chain of adders, starting from the last one.
   declare
      use Wire_Maps;

      I : Cursor := Wires.Floor ("z99");

      --  Last z bit (e.g., z45) is C-out of the last full adder.
      Carry   : Wire_Name := I.Key;
      X, Y, Z : Wire_Name;
   begin
      loop
         I := I.Previous;
         Z := I.Key;
         X := Wire_Name'['x', Z (2), Z (3)];
         Y := Wire_Name'['y', Z (2), Z (3)];

         if Z = "z00" then
            if not Half_Adder (Wires, X, Y, Z, Carry) then
               raise Constraint_Error with Z;
            end if;
         else
            if not Full_Adder (Wires, X, Y, Z, Carry, Carry) then
               raise Constraint_Error with Z;
            end if;
         end if;

         exit when Z = "z00";
      end loop;
   end;

   --  Sort swapped names and format the result.
   declare
      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Positive, Wire_Name, Wire_Name_Array);
   begin
      Sort (Swapped);
      for I in Swapped'Range loop
         Put (Swapped (I));
         if I < Swapped'Last then
            Put (',');
         end if;
      end loop;
      New_Line;
   end;
end Day_24_2;
