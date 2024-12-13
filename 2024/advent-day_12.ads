with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_12 is
   subtype Plant_Type is Character range 'A' .. 'Z';
   type Garden is array (Positive range <>, Positive range <>) of Plant_Type;
   type Visited_Map is array (Positive range <>, Positive range <>) of Boolean;

   procedure Analyze
     (Plants : in Garden; Row, Col : in Positive; Visited : in out Visited_Map;
      Area, Perimeter :    out Natural) with
     Pre =>
      Row in Plants'Range (1) and Col in Plants'Range (2) and
      Plants'First (1) = Visited'First (1) and
      Plants'Last (1) = Visited'Last (1) and
      Plants'First (2) = Visited'First (2) and
      Plants'Last (2) = Visited'Last (2) and not Visited (Row, Col);

   function Input (File : File_Type) return Garden;
end Advent.Day_12;
