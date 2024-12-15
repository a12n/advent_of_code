with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_14 is
   --  type X_Position is mod 11;
   --  type Y_Position is mod 7;
   type X_Position is mod 101;
   type Y_Position is mod 103;

   type Position is record
      X : X_Position;
      Y : Y_Position;
   end record;

   type Velocity is record
      X : Integer;
      Y : Integer;
   end record;

   type Robot_Type is record
      P : Position;
      V : Velocity;
   end record;

   type Robot_Array is array (Positive range <>) of Robot_Type;

   function Distance (P, Q : Position) return Natural;
   function Distance (Robots : Robot_Array) return Natural;
   function Input (File : File_Type) return Robot_Array;
   procedure Print (File : File_Type; Robots : Robot_Array);
   procedure Simulate (Robots : in out Robot_Array; Time : Positive);
end Advent.Day_14;
