with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_15 is
   type Tile is (Empty, Wall, Box, Robot);
   type Warehouse_Map is array (Positive range <>, Positive range <>) of Tile;

   function GPS_Coordinate (Pos : Position) return Natural is
     (100 * (Pos (1) - 1) + (Pos (2) - 1));

   function Get_Move (File : File_Type) return Direction;

   function Get_Warehouse
     (File : File_Type; Robot_Pos : out Position) return Warehouse_Map;

   function Move
     (Warehouse : in out Warehouse_Map; Pos : in out Position; Dir : Direction)
      return Boolean with
     Pre  =>
      Pos (1) in Warehouse'Range (1) and Pos (2) in Warehouse'Range (2) and
      Warehouse (Pos (1), Pos (2)) in Box .. Robot,
     Post => (if Move'Result then Warehouse (Pos (1), Pos (2)) = Empty);
end Advent.Day_15;
