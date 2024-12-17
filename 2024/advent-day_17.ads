with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_17 is
   type Number is range 0 .. 7;
   type Number_Array is array (Positive range <>) of Number;
   type Register is mod 2**64;
   type Register_Name is (A, B, C);
   type Register_Set is array (Register_Name) of Register;

   type CPU_Type is record
      R : Register_Set := [others => 0];
      I : Natural      := 0;
   end record;

   function Get_CPU (File : File_Type) return CPU_Type;
   function Get_Program (File : File_Type) return Number_Array;
   function Run
     (CPU : in out CPU_Type; Program : Number_Array) return Number_Array;
   function To_String (Numbers : Number_Array) return String;
end Advent.Day_17;
