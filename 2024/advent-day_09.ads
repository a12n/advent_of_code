with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_09 is
   type ID_Type is range 0 .. 20_000;
   type Block_Size is range 0 .. 9;
   type Link_Type is range 0 .. 20_000;
   type Checksum_Type is range 0 .. 2**64;

   type Block is record
      ID       : ID_Type;
      Space    : Boolean;
      Size     : Block_Size;
      Next     : Link_Type := 0;
      Previous : Link_Type := 0;
   end record with
     Size => 64;
   type Block_Array is array (Positive range <>) of Block;

   package Block_Size_Text_IO is new Ada.Text_IO.Integer_IO (Block_Size);
   package Checksum_Text_IO is new Ada.Text_IO.Integer_IO (Checksum_Type);

   function Checksum (Blocks : Block_Array) return Natural;

   function Input (File : File_Type) return Block_Array;

   procedure Rearrange (Blocks : in out Block_Array);
end Advent.Day_09;
