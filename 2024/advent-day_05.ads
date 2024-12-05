with Ada.Strings.Text_Buffers; use Ada.Strings.Text_Buffers;
with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_05 is
   subtype Page_Number is Positive range 1 .. 99;
   type Precedence is array (Page_Number, Page_Number) of Ternary with Put_Image => Put_Image;
   type Page_Array is array (Positive range <>) of Page_Number;

   function In_Order (Order : Precedence; Pages : Page_Array) return Boolean;
   function Input_Pages (File : File_Type) return Page_Array;
   function Input_Precedence (File : File_Type) return Precedence;

   procedure Put_Image (Buffer : in out Root_Buffer_Type'Class; Order : in Precedence);
end Advent.Day_05;
