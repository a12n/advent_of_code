with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_13 is
   --  Button A: X+94, Y+34
   --  Button B: X+22, Y+67
   --  Prize: X=8400, Y=5400
   --
   --  A_x n₁ + B_x n₂ = P_x
   --  A_y n₁ + B_y n₂ = P_y
   --
   --  n₁ = (P_x - B_x n₂) / A_x
   --  A_y n₁ + B_y n₂ = P_y
   --
   --  n₂ = (P_x - A_x n₁) / B_x
   --  A_y n₁ + B_y n₂ = P_y
   --
   --  n₁ = (P_y - (B_y P_x) / B_x) / (A_y - (B_y A_x) / B_x)
   --  n₂ = (P_y - (A_y P_x) / A_x) / (B_y + (A_y B_x) / A_x)

   --
   --  [ax bx; [n; = [x y]
   --   ay by]  m]
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

   type Offset is range 0 .. 100;
   type Position is range -2**63 .. 2**63;
   type Counter is range 0 .. 2**64;

   package Counter_Text_IO is new Ada.Text_IO.Integer_IO (Counter);

   type Button_Behavior is record
      X, Y : Offset;
   end record;

   type Prize_Position is record
      X, Y : Position;
   end record;

   type Claw_Machine is record
      A, B  : Button_Behavior;
      Prize : Prize_Position;
   end record;

   type Push_Count is record
      A, B : Counter;
   end record;

   function Cost (Pushes : Push_Count) return Counter is
     (3 * Pushes.A + Pushes.B);
   function Input_Entry (File : File_Type) return Claw_Machine;
   function Solution
     (Machine : in Claw_Machine; Pushes : out Push_Count) return Boolean;
end Advent.Day_13;
