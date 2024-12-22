with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_21 is
   type Numeric_Key is ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A');
   type Directional_Key is ('v', '<', '>', '^', 'A');
   type Numeric_Presses is array (Positive range <>) of Numeric_Key;
   type Directional_Presses is array (Positive range <>) of Directional_Key;

  function Get_Code (File : File_Type) return Numeric_Presses with
     Post =>
      Get_Code'Result'Length = 4 and
      (for all I in 1 .. 3 => Get_Code'Result (I) in '0' .. '9') and
      Get_Code'Result (4) = 'A';
end Advent.Day_21;
