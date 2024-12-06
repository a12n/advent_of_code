with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_06 is
   type Obstruction_Map is
     array (Positive range <>, Positive range <>) of Boolean;
   type Visited_Map is array (Positive range <>, Positive range <>) of Boolean;

   type Direction is (Down, Left, Right, Up);
   type Position is array (1 .. 2) of Positive;

   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Obstruction_Map;

   function Walk
     (Obstructions : Obstruction_Map; Guard : Position; Guard_Dir : Direction)
      return Visited_Map with
     Pre =>
      Guard (1) in Obstructions'Range (1) and
      Guard (2) in Obstructions'Range (2);
end Advent.Day_06;
