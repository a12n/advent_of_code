with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_16 is
   type Tile_Type is (Empty, Wall);
   type Maze_Type is array (Positive range <>, Positive range <>) of Tile_Type;
   pragma Pack (Maze_Type);

   type Visited_Map is
     array
       (Positive range <>, Positive range <>, Direction range <>) of Boolean;

   function Best_Path
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction) return Natural;

   function Get_Maze
     (File : File_Type; Start_Pos, Finish_Pos : out Position) return Maze_Type;

   procedure Print (File : File_Type; Maze : Maze_Type; Visited : Visited_Map);
end Advent.Day_16;
