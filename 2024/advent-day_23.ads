with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_23 is
   subtype Address_Char is Character range 'a' .. 'z';
   type String_Address is array (1 .. 2) of Address_Char;

   type Address is range 0 .. (25 * 26 + 25); --  From "aa" to "zz"
   type Connection_Map is array (Address, Address) of Boolean;

   function Get_Connections (File : File_Type) return Connection_Map;

   function To_Address (Addr : String_Address) return Address is
     ((Address_Char'Pos (Addr (1)) - Address_Char'Pos ('a')) * 26 +
      Address_Char'Pos (Addr (2)) - Address_Char'Pos ('a'));
end Advent.Day_23;
