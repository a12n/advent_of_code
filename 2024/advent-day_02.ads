with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

package Advent.Day_02 is
   subtype Level_Type is Positive range 1 .. 99;
   type Level_Array is array (Positive range <>) of Level_Type;
   type Difference_Array is array(Positive range <>) of Integer;

   function Difference (Levels : Level_Array) return Difference_Array;
   function Input_Entry (File : File_Type) return Level_Array;
   function Is_Monotone (Difference : Difference_Array;
                         Min, Max : Integer;
                         Dampener : Boolean) return Boolean;

   package Level_Vectors is new Ada.Containers.Vectors (Positive, Level_Type);

   Input_Error : exception;

   procedure Get_Input_Entry (Levels : out Level_Vectors.Vector);
   function Is_Monotone (Levels   : Level_Vectors.Vector;
                         Min, Max : Integer;
                         Dampener : Boolean) return Boolean;
end Advent.Day_02;
