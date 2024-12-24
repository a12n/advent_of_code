with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

package Advent.Day_24 is
   subtype Wire_Name is String (1 .. 3);

   type Gate_Type is ('0', '1', '&', '|', '^');
   type Wire_Type is record
      Gate : Gate_Type;
      A, B : Wire_Name := "   ";
   end record;

   type Output_Type is mod 2**46;

   package Output_Text_IO is new Ada.Text_IO.Modular_IO (Output_Type);

   package Wire_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Wire_Name, Element_Type => Wire_Type);
   subtype Wire_Map is Wire_Maps.Map;

   function Get_Wires (File : File_Type) return Wire_Map;

   function Output (Wires : Wire_Map) return Output_Type;

   function Signal (Wires : Wire_Map; Name : Wire_Name) return Boolean;
end Advent.Day_24;
