with Ada.Containers.Vectors;

package Advent.Day_02 is
   subtype Level_Type is Positive range 1 .. 99;

   package Level_Vectors is new Ada.Containers.Vectors (Positive, Level_Type);

   Input_Error : exception;

   procedure Get_Input_Entry (Levels : out Level_Vectors.Vector);
   function Is_Monotone (Levels   : Level_Vectors.Vector;
                         Min, Max : Integer) return Boolean;
end Advent.Day_02;
