with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_15 is
   type Warehouse_Map is
     array (Positive range <>, Positive range <>) of Character;

   function Get_Move (File : File_Type) return Direction;
   function Get_Warehouse
     (File : File_Type; Robot : out Position) return Warehouse_Map;
end Advent.Day_15;
