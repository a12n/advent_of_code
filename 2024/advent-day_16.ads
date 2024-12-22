with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_16 is
   type Tile_Type is (Empty, Wall);
   type Maze_Type is array (Positive range <>, Positive range <>) of Tile_Type;
   pragma Pack (Maze_Type);

   function Best_Path
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction) return Natural;

   function Best_Paths
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction; Finish_Cost : out Natural) return Maze_Type;

   function Get_Maze
     (File : File_Type; Start_Pos, Finish_Pos : out Position)
      return Maze_Type with
     Post =>
      Start_Pos (1) in Get_Maze'Result'Range (1) and
      Start_Pos (2) in Get_Maze'Result'Range (2) and
      Finish_Pos (1) in Get_Maze'Result'Range (1) and
      Finish_Pos (2) in Get_Maze'Result'Range (2);

   procedure Print (File : File_Type; Maze : Maze_Type);
   procedure Print
     (File : File_Type; Maze, Paths : Maze_Type; Empty_Char : Character := '.';
      Wall_Char : Character := '#'; Path_Char : Character := 'O');
end Advent.Day_16;
