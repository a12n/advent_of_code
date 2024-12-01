with Ada.Containers.Vectors;

package Advent.Day_01 is
   subtype Number is Natural range 1 .. 99_999;

   package Number_Vectors is new Ada.Containers.Vectors (Positive, Number);

   Input_Error : exception;

   procedure Get_Input (Left, Right : out Number_Vectors.Vector);
end Advent.Day_01;
