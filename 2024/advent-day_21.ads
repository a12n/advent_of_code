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
   type Numeric_Keys is array (Positive range <>) of Numeric_Key;

   function To_Character (Key : Numeric_Key) return Character is
     (case Key is
        when '0' .. '9' =>
          Character'Val (Numeric_Key'Pos (Key) + Character'Pos ('0')),
        when 'A' => 'A');

   Numeric_Keys_Capacity : constant := 4;
   type Bounded_Numeric_Keys is record
      Length   : Natural;
      Elements : Numeric_Keys (1 .. Numeric_Keys_Capacity);
   end record;

   type Numeric_Keys_Array is
     array (Positive range <>) of Bounded_Numeric_Keys;

   function To_Bounded (Keys : Numeric_Keys) return Bounded_Numeric_Keys;
   function To_String (Keys : Numeric_Keys) return String;

   --      +---+---+
   --      | ^ | A |
   --  +---+---+---+
   --  | < | v | > |
   --  +---+---+---+
   type Directional_Key is ('v', '<', '>', '^', 'A');
   type Directional_Keys is array (Positive range <>) of Directional_Key;

   function To_Character (Key : Directional_Key) return Character is
     (case Key is when 'v' => 'v', when '<' => '<', when '>' => '>',
        when '^' => '^', when 'A' => 'A');

   Directional_Keys_Capacity : constant := 4 * 5 * 3 * 3;
   type Bounded_Directional_Keys is record
      Length   : Natural;
      Elements : Directional_Keys (1 .. Directional_Keys_Capacity);
   end record;

   type Directional_Keys_Array is
     array (Positive range <>) of Bounded_Directional_Keys;

   function To_Bounded
     (Keys : Directional_Keys) return Bounded_Directional_Keys;
   function To_String (Keys : Directional_Keys) return String;

   function Valid_Code (Keys : Numeric_Keys) return Boolean is
     (Keys'Length = 4 and then (for all I in 1 .. 3 => Keys (I) in '0' .. '9')
      and then Keys (4) = 'A');

   function Get_Code (File : File_Type) return Numeric_Keys with
     Post => Valid_Code (Get_Code'Result);

   function Revert (Key : Directional_Key) return Directional_Key is
     (case Key is when 'v' => '^', when '<' => '>', when '>' => '<',
        when '^' => 'v', when 'A' => 'A');

   function Revert (Keys : Directional_Keys) return Directional_Keys with
     Post => Revert'Result'Length = Keys'Length;

   function To_Number (Code : Numeric_Keys) return Natural with
     Pre => Valid_Code (Code), Post => To_Number'Result < 1_000;

   --  Button presses needed on the corresponding directional keypad
   --  to move from one button to another on the numeric keypad.
   function Translate (From, To : Numeric_Key) return Directional_Keys with
     Post => Translate'Result'Length <= 5;
   function Translate (Keys : Numeric_Keys) return Directional_Keys;
   function Translate
     (Keys : Numeric_Keys; Current : in out Numeric_Key)
      return Directional_Keys;

   --  Button presses needed on the corresponding directional keypad
   --  to move on the second order directional keypad.
   function Translate (From, To : Directional_Key) return Directional_Keys with
     Post => Translate'Result'Length <= 3;
   function Translate (Keys : Directional_Keys) return Directional_Keys;
   function Translate
     (Keys : Directional_Keys; Current : in out Directional_Key)
      return Directional_Keys;
end Advent.Day_21;
