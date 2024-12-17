with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_17 is
   type Number is range 0 .. 7;
   type Register is range 0 .. 2**64;
   type Number_Array is array (Positive range <>) of Number;

   type CPU_Type is record
      A, B, C : Register := 0;
      PC      : Natural  := 0;
   end record;

   function Get_CPU (File : File_Type) return CPU_Type;
   function Get_Program (File : File_Type) return Number_Array;
   function Run
     (CPU : in out CPU_Type; Program : Number_Array) return Number_Array;
   function To_String (Numbers : Number_Array) return String;
end Advent.Day_17;
