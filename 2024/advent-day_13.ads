with Ada.Text_IO;  use Ada.Text_IO;

package Advent.Day_13 is
   --  Button A: X+94, Y+34
   --  Button B: X+22, Y+67
   --  Prize: X=8400, Y=5400
   --
   --  A = (94, 34)
   --  B = (22, 67)
   --  Prize = (8400, 5400)
   --  Prize = n * A + m * B
   --  (8400, 5400) = n (94, 34) + m (22, 67)
   --  Cost = 3 n + 1 m
   --
   --  n ∈ [0, 100], m ∈ [0, 100]
   --  min Cost
   --
   --  Maybe infeasible.

   type Push_Count is record
      A, B : Natural range 0 .. 100;
   end record;

   type Offset is record
      X, Y : Natural range 0 .. 100;
   end record;

   type Position is record
      X, Y : Natural range 0 .. 30_000;
   end record;

   type Claw_Machine is record
      A, B  : Offset;
      Prize : Position;
   end record;

   function Cost (Pushes : Push_Count) return Natural is
     (3 * Pushes.A + Pushes.B);
   function Input_Entry (File : File_Type) return Claw_Machine;
   function Solution
     (Machine : in Claw_Machine; Pushes : out Push_Count) return Boolean;
end Advent.Day_13;
