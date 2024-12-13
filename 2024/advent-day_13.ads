with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

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

   type Claw_Machine is record
      A, B  : Offset;
      Prize : Position;
   end record;

   function Input_Entry (File : File_Type) return Claw_Machine;
end Advent.Day_13;
