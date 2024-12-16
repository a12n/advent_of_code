with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_16 is
   type Tile_Type is (Empty, Wall);
   type Maze_Type is array (Positive range <>, Positive range <>) of Tile_Type;
   pragma Pack (Maze_Type);

   function Get_Maze
     (File : File_Type; Start, Finish : out Position) return Maze_Type;
end Advent.Day_16;
