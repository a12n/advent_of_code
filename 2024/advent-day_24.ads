with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

package Advent.Day_24 is
   subtype Wire_Name is String (1 .. 3);
   type Wire_Name_Array is array (Positive range <>) of Wire_Name;

   Empty_Name : constant Wire_Name := "   ";

   type Gate_Type is ('0', '1', '&', '|', '^');
   type Wire_Type is record
      Gate : Gate_Type := '0';
      A, B : Wire_Name := Empty_Name;
   end record;

   type Number_Type is mod 2**46;

   package Number_Text_IO is new Ada.Text_IO.Modular_IO (Number_Type);

   package Wire_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Wire_Name, Element_Type => Wire_Type);
   subtype Wire_Map is Wire_Maps.Map;

   function Get_Wires (File : File_Type) return Wire_Map;

   function Number (Wires : Wire_Map; ID : Character) return Number_Type;

   function Signal (Wires : Wire_Map; Name : Wire_Name) return Boolean;

   --  Match the specified part of the wires as a full adder. Returns
   --  True if it's a valid full adder. Carry-in wire is saved to
   --  C_in.
   function Full_Adder
     (Wires : Wire_Map; A, B, S, C_out : Wire_Name; C_in : out Wire_Name)
      return Boolean;

   function Half_Adder
     (Wires : Wire_Map; A, B, S, C_out : Wire_Name) return Boolean;
end Advent.Day_24;
