with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_10 is
   type Height is range 0 .. 9;
   type Height_Map is array (Positive range <>, Positive range <>) of Height;
   type Peak_Map is array (Positive range <>, Positive range <>) of Boolean;

   function Input (File : File_Type) return Height_Map;

   function Number_Trails
     (Heights : Height_Map; Pos : Position) return Natural with
     Pre => Pos (1) in Heights'Range (1) and Pos (2) in Heights'Range (2);

   function Peaks (Heights : Height_Map; Pos : Position) return Peak_Map with
     Pre => Pos (1) in Heights'Range (1) and Pos (2) in Heights'Range (2);

   function Score (Peaks : Peak_Map) return Natural;
end Advent.Day_10;
