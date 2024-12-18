with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_16 is
   type Tile_Type is (Empty, Wall);
   type Maze_Type is array (Positive range <>, Positive range <>) of Tile_Type;
   pragma Pack (Maze_Type);
   type Tile_Map is array (Positive range <>, Positive range <>) of Boolean;

   function Best_Path
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction) return Natural;

   function Best_Paths
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction; Cost : out Natural) return Tile_Map;

   function Get_Maze
     (File : File_Type; Start_Pos, Finish_Pos : out Position) return Maze_Type;

   function Number_Best_Tiles
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction; Finish_Cost : Natural) return Natural;

   procedure Print (File : File_Type; Maze : Maze_Type);
end Advent.Day_16;
