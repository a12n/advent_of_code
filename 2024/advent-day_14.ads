with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_14 is
   --  For sample input.
   type X_Position is mod 11;
   type Y_Position is mod 7;

   --  --  For actual input.
   --  type X_Position is mod 101;
   --  type Y_Position is mod 103;

   type Position is record
      X : X_Position;
      Y : Y_Position;
   end record;

   type Velocity is record
      X : Integer;
      Y : Integer;
   end record;

   type Robot is record
      P : Position;
      V : Velocity;
   end record;

   type Robot_Array is array (Positive range <>) of Robot;

   function Input (File : File_Type) return Robot_Array;
end Advent.Day_14;
