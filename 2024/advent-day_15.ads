with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_15 is
   Box_Tile   : constant Character := 'O';
   Robot_Tile : constant Character := '@';
   Wall_Tile  : constant Character := '#';

   type Warehouse_Map is
     array (Positive range <>, Positive range <>) of Character;

   function GPS_Coordinate (Pos : Position) return Natural is
     (100 * (Pos (1) - 1) + (Pos (2) - 1));
   function Get_Move (File : File_Type) return Direction;
   function Get_Warehouse
     (File : File_Type; Robot : out Position) return Warehouse_Map;
   function Move
     (Warehouse : in out Warehouse_Map; Pos : in out Position; Dir : Direction)
      return Boolean;
end Advent.Day_15;
