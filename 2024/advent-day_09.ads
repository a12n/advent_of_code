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

   function Checksum (Blocks : Block_Array) return Checksum_Type;

   function Input (File : File_Type) return Block_Array;

   procedure Rearrange (Blocks : in out Block_Array);

   --  type Block_ID is range -1 .. 20_000 with
   --    Size => 16;
   --  type Block_Array2 is array (Positive range <>) of Block_ID;
   --  type Index_Array is array (Positive range <>) of Natural;
   --
   --  function Is_File (Block : Block_ID) return Boolean is (Block >= 0);
   --  function Is_Space (Block : Block_ID) return Boolean is (Block < 0);
   --  function Input (File : File_Type) return Block_Array2;
   --  function Rearrange (Blocks : Block_Array2) return Index_Array with
   --    Post => Rearrange'Result'Length = Blocks'Length;
   --  function Checksum
   --    (Blocks : Block_Array2; Indices : Index_Array) return Checksum_Type with
   --    Pre => Blocks'Length = Indices'Length;
end Advent.Day_09;
