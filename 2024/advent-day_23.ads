with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_23 is
   subtype Address_Char is Character range 'a' .. 'z';
   type String_Address is array (1 .. 2) of Address_Char;

   type Address is range 0 .. (25 * 26 + 25); --  From "aa" to "zz"
   type Address_Set is array (Address) of Boolean;
   type Connection_Map is array (Address) of Address_Set;
   pragma Pack (Address_Set);

   function Empty (Set : Address_Set) return Boolean is
     (for all I in Set'Range => not Set (I));
   function Length (Set : Address_Set) return Natural;

   function To_Address (Addr : String_Address) return Address is
     ((Address_Char'Pos (Addr (1)) - Address_Char'Pos (Address_Char'First)) *
      26 +
      Address_Char'Pos (Addr (2)) - Address_Char'Pos (Address_Char'First));

   subtype Historian_Address is
     Address range To_Address ("ta") .. To_Address ("tz");

   function Get_Connections (File : File_Type) return Connection_Map;

   --  A host is connected to the network if it has connection to
   --  itself.
   function Online
     (Connections : Connection_Map; Addr : Address) return Boolean is
     (Connections (Addr) (Addr));

   function To_String_Address (Addr : Address) return String_Address is
     ([Address_Char'Val (Addr / 26 + Address_Char'Pos (Address_Char'First)),
      Address_Char'Val (Addr mod 26 + Address_Char'Pos (Address_Char'First))]);

   function To_String (Addr : String_Address) return String is
     ([Addr (1), Addr (2)]);
   function To_String (Addr : Address) return String is
     (To_String (To_String_Address (Addr)));
end Advent.Day_23;
