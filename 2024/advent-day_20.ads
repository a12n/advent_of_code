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

   function Get_Racetrack
     (File : File_Type; Start_Pos, Finish_Pos : out Position)
      return Racetrack_Type renames
     Day_16.Get_Maze;

   function Number_Cheats
     (Track : Racetrack_Type; Distance : Distance_Map; Path : Position_Array;
      Cheat : not null access function
        (P, Q : Position; Old_Dist, New_Dist : Natural) return Boolean)
      return Natural with
     Pre =>
      Distance'Length (1) = Track'Length (1) and
      Distance'Length (2) = Track'Length (2) and Path'Length > 0 and
      (for all Pos of Path => Track (Pos (1), Pos (2)) = Empty) and
      (for all I in Path'First + 1 .. Path'Last =>
         Distance (Path (I) (1), Path (I) (2)) =
         Distance (Path (I - 1) (1), Path (I - 1) (2)) + 1);

   --  Find shortest path and fill distance and previous position
   --  maps. Returns False if there's no path.
   function Shortest_Path
     (Track    :     Racetrack_Type; Start_Pos, Finish_Pos : Position;
      Previous : out Previous_Map; Distance : out Distance_Map)
      return Boolean with
     Pre =>
      Start_Pos (1) in Track'Range (1) and Start_Pos (2) in Track'Range (2) and
      Finish_Pos (1) in Track'Range (1) and
      Finish_Pos (2) in Track'Range (2) and
      Previous'Length (1) = Track'Length (1) and
      Previous'Length (2) = Track'Length (2) and
      Distance'Length (1) = Track'Length (1) and
      Distance'Length (2) = Track'Length (2);

   --  Find shortest path and get distance from start to finish.
   function Shortest_Path_Length
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position) return Natural;

   --  Backtrack shortest path positions from the previous map of a
   --  found shortest path.
   function Shortest_Path_Positions
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
   function Shortest_Path_Positions
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position)
      return Position_Array;

   procedure Print
     (File       : File_Type; Track, Paths : Racetrack_Type;
      Empty_Char : Character := '.'; Wall_Char : Character := '#';
      Path_Char  : Character := 'O') renames
     Day_16.Print;
end Advent.Day_20;
