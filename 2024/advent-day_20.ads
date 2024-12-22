with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Day_16;
with Advent.Grids; use Advent.Grids;

package Advent.Day_20 is
   subtype Racetrack_Type is Day_16.Maze_Type;

   function Get_Racetrack
     (File : File_Type; Start_Pos, Finish_Pos : out Position)
      return Racetrack_Type renames
     Day_16.Get_Maze;

   function Shortest_Path_Length
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position) return Natural;

   procedure Print
     (File       : File_Type; Track, Paths : Racetrack_Type;
      Empty_Char : Character := '.'; Wall_Char : Character := '#';
      Path_Char  : Character := 'O') renames
     Day_16.Print;
end Advent.Day_20;
