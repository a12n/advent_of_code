with Ada.Text_IO; use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

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

   procedure Copy (Dest : in out Visited_Map; Source : Visited_Map);

   function Edge
     (Polygon : Visited_Map; Row, Col : Positive; Side : Direction)
      return Boolean with
     Pre => Row in Polygon'Range (1) and Col in Polygon'Range (2);

   function Flood_Fill
     (Plants : Garden; Row, Col : Positive) return Visited_Map;
   function Flood_Fill
     (Plants : Garden; Row, Col : Positive; Area : out Natural)
      return Visited_Map;
   function Flood_Fill
     (Plants : Garden; Row, Col : Positive; Area, Perimeter : out Natural)
      return Visited_Map;

   function Number_Sides (Polygon : Visited_Map) return Positive;

   function Input (File : File_Type) return Garden;
end Advent.Day_12;
