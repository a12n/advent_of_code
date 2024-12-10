with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_09 is
   type Block_ID is range -1 .. 20_000 with Size => 16;
   type Block_Size is range 0 .. 9;

   type Block is record
      ID   : Block_ID;
      Size : Block_Size;
   end record with
     Size => 32;

   type Block_Array is array (Positive range <>) of Block;
   type ID_Array is array (Positive range <>) of Block_ID;
   type Size_Array is array (Positive range <>) of Natural;

   type Checksum_Type is range 0 .. 2**64;

   package Checksum_Text_IO is new Ada.Text_IO.Integer_IO (Checksum_Type);

   function Checksum (Blocks : Block_Array) return Checksum_Type;
   function Checksum (Blocks : ID_Array) return Checksum_Type;

   function File_Block (Block : Block_ID) return Boolean is (Block /= -1);

   function Input (File : File_Type) return Size_Array;

   procedure Rearrange (Blocks : in out Block_Array);
   procedure Rearrange (Blocks : in out ID_Array);

   function Space_Block (Block : Block_ID) return Boolean is (Block = -1);

   function To_Blocks (Sizes : Size_Array) return Block_Array;
   function To_Blocks (Sizes : Size_Array) return ID_Array;
end Advent.Day_09;
