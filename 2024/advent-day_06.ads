with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_06 is
   type Boolean_Grid is
     array (Positive range <>, Positive range <>) of Boolean;
   type Polygonal_Chain is array (Positive range <>) of Position;
   type Position_Array is array (Positive range <>) of Position;

   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Boolean_Grid with
     Post =>
      Guard (1) in Input'Result'Range (1) and
      Guard (2) in Input'Result'Range (2) and
      not Input'Result (Guard (1), Guard (2));

   function Walk
     (Blocked : Boolean_Grid; Guard : Position; Guard_Dir : Direction)
      return Boolean_Grid with
     Pre =>
      Guard (1) in Blocked'Range (1) and Guard (2) in Blocked'Range (2) and
      not Blocked (Guard (1), Guard (2));

   function Walk
     (Blocked : Boolean_Grid; Guard : Position; Guard_Dir : Direction)
      return Polygonal_Chain with
     Pre =>
      Guard (1) in Blocked'Range (1) and Guard (2) in Blocked'Range (2) and
      not Blocked (Guard (1), Guard (2));

   function Loop_Obstructions
     (Path : Polygonal_Chain) return Position_Array with
     Pre => Path'Length > 0;
end Advent.Day_06;
