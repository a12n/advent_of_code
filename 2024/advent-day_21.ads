with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_21 is
   --  +---+---+---+
   --  | 7 | 8 | 9 |
   --  +---+---+---+
   --  | 4 | 5 | 6 |
   --  +---+---+---+
   --  | 1 | 2 | 3 |
   --  +---+---+---+
   --      | 0 | A |
   --      +---+---+
   type Numeric_Key is ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A');
   type Numeric_Presses is array (Positive range <>) of Numeric_Key;

   --      +---+---+
   --      | ^ | A |
   --  +---+---+---+
   --  | < | v | > |
   --  +---+---+---+
   type Directional_Key is ('v', '<', '>', '^', 'A');
   type Directional_Presses is array (Positive range <>) of Directional_Key;

   function Valid_Code (Presses : Numeric_Presses) return Boolean is
     (Presses'Length = 4
      and then (for all I in 1 .. 3 => Presses (I) in '0' .. '9')
      and then Presses (4) = 'A');

   function Get_Code (File : File_Type) return Numeric_Presses with
     Post => Valid_Code (Get_Code'Result);

   function To_Number (Code : Numeric_Presses) return Natural with
     Pre => Valid_Code (Code), Post => To_Number'Result < 1_000;

   --  Button presses needed on the corresponding directional keypad
   --  to move from one button to another on the numeric keypad.
   function Translate (From, To : Numeric_Key) return Directional_Presses;
   function Translate (Presses : Numeric_Presses) return Directional_Presses;

   --  Button presses needed on the corresponding directional keypad
   --  to move on the second order directional keypad.
   function Translate (From, To : Directional_Key) return Directional_Presses;
   function Translate
     (Presses : Directional_Presses) return Directional_Presses;
end Advent.Day_21;
