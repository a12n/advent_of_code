with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_19 is
   type Color_Type is (White, Blue, Black, Red, Green);
   type Stripe_Array is array (Positive range <>) of Color_Type;
   type Towel_Type is record
      Stripes : Stripe_Array (1 .. 8);
      N       : Positive range 1 .. 8;
   end record;
   type Towel_Array is array (Positive range <>) of Towel_Type;

   function Get_Design (File : File_Type) return Stripe_Array;
   function Get_Patterns (File : File_Type) return Towel_Array;
end Advent.Day_19;
