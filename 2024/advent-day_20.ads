with Ada.Text_IO; use Ada.Text_IO;
with Advent.Day_16;
use all type Advent.Day_16.Tile_Type;
with Advent.Grids; use Advent.Grids;

package Advent.Day_20 is
   subtype Racetrack_Type is Day_16.Maze_Type;

   --  Distance from the origin position to a position on the track.
   type Distance_Map is
     array (Positive range <>, Positive range <>) of Natural;

   --  Direction to the previous position in a shortest path.
   type Previous_Map is
     array (Positive range <>, Positive range <>) of Maybe_Direction;

   procedure Find_Shortest_Path
     (Track    :     Racetrack_Type; Start_Pos, Finish_Pos : Position;
      Previous : out Previous_Map; Distance : out Distance_Map) with
     Pre =>
      Start_Pos (1) in Track'Range (1) and Start_Pos (2) in Track'Range (2) and
      Finish_Pos (1) in Track'Range (1) and
      Finish_Pos (2) in Track'Range (2) and
      Previous'Length (1) = Track'Length (1) and
      Previous'Length (2) = Track'Length (2) and
      Distance'Length (1) = Track'Length (1) and
      Distance'Length (2) = Track'Length (2);

   function Get_Racetrack
     (File : File_Type; Start_Pos, Finish_Pos : out Position)
      return Racetrack_Type renames
     Day_16.Get_Maze;

   --  Backtrack shortest path positions from the previous map of a
   --  found shortest path.
   function Shortest_Path
     (Previous : Previous_Map; Start_Pos, Finish_Pos : Position)
      return Position_Array with
     Pre =>
      Start_Pos (1) in Previous'Range (1) and
      Start_Pos (2) in Previous'Range (2) and
      Finish_Pos (1) in Previous'Range (1) and
      Finish_Pos (2) in Previous'Range (2) and
      Previous (Start_Pos (1), Start_Pos (2)) = None and
      Previous (Finish_Pos (1), Finish_Pos (2)) /= None;

   --  Find shortest path and then backtrack path positions.
   function Shortest_Path
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position)
      return Position_Array;

   --  Find shortest path and get distance from start to finish.
   function Shortest_Path_Length
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position) return Natural;

   procedure Print
     (File       : File_Type; Track, Paths : Racetrack_Type;
      Empty_Char : Character := '.'; Wall_Char : Character := '#';
      Path_Char  : Character := 'O') renames
     Day_16.Print;
end Advent.Day_20;
