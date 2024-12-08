with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_06 is
   type Obstruction_Map is
     array (Positive range <>, Positive range <>) of Boolean;
   type Visited_Map is array (Positive range <>, Positive range <>) of Boolean;

   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Obstruction_Map with
     Post =>
      Guard (1) in Input'Result'Range (1) and
      Guard (2) in Input'Result'Range (2) and
      not Input'Result (Guard (1), Guard (2));

   function Walk
     (Obstructions : Obstruction_Map; Guard : Position; Guard_Dir : Direction)
      return Visited_Map with
     Pre =>
      Guard (1) in Obstructions'Range (1) and
      Guard (2) in Obstructions'Range (2) and
      not Obstructions (Guard (1), Guard (2));
end Advent.Day_06;
