with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_23 is
   subtype Address_Char is Character range 'a' .. 'z';
   type String_Address is array (1 .. 2) of Address_Char;

   type Address is range 0 .. (25 * 26 + 25); --  From "aa" to "zz"
   type Connection_Map is array (Address, Address) of Boolean;

   --  Connected components of the network.
   type Component_Map is array (Address) of Positive;
   type Component_Size_Array is array (Positive range <>) of Natural;

   --  Analyzes connected components of the network. If
   --  Components(Addr) is 3, then address belongs to the component
   --  number 3. The number of nodes in the component 3 is in
   --  Connected_Components'Result(3).
   function Connected_Components
     (Connections : Connection_Map; Components : out Component_Map)
      return Component_Size_Array with
     Post =>
      (for all I in Address'Range =>
         Components (I) <= Connected_Components'Result'Length or
         Components (I) = Positive'Last);

   function Get_Connections (File : File_Type) return Connection_Map;

   --  A host is connected to the network if it has connection to
   --  itself.
   function Online
     (Connections : Connection_Map; Addr : Address) return Boolean is
     (Connections (Addr, Addr));

   function To_Address (Addr : String_Address) return Address is
     ((Address_Char'Pos (Addr (1)) - Address_Char'Pos (Address_Char'First)) *
      26 +
      Address_Char'Pos (Addr (2)) - Address_Char'Pos (Address_Char'First));

   function To_String_Address (Addr : Address) return String_Address is
     ([Address_Char'Val (Addr / 26 + Address_Char'Pos (Address_Char'First)),
      Address_Char'Val (Addr mod 26 + Address_Char'Pos (Address_Char'First))]);
end Advent.Day_23;
