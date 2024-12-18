with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_18 is
   type Corrupted_Map is array (Natural range <>, Natural range <>) of Boolean;

   function Get_Byte_Position (File : File_Type) return Position;

   procedure Print (File : File_Type; Corrupted : Corrupted_Map);

   function Shortest_Path
     (Corrupted :     Corrupted_Map; Start_Pos, Finish_Pos : Position;
      Distance  : out Natural) return Boolean with
     Pre =>
      Start_Pos (1) in Corrupted'Range (1) and
      Start_Pos (2) in Corrupted'Range (2) and
      Finish_Pos (1) in Corrupted'Range (1) and
      Finish_Pos (2) in Corrupted'Range (2);
end Advent.Day_18;
