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
   package Numeric is
      type Key_Type is ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A');
      type Key_Set is array (Key_Type) of Boolean;
      type Key_Array is array (Positive range <>) of Key_Type;
      type Bounded_Key_Array is record
         Length   : Natural := 0;
         Elements : Key_Array (1 .. 8);
      end record;
      type Key_Array_List is array (Positive range <>) of Bounded_Key_Array;

      subtype Code_Type is Key_Array (1 .. 4) with
          Dynamic_Predicate =>
           (for all I in 1 .. 3 => Code_Type (I) in '0' .. '9') and
           Code_Type (4) = 'A';

      function Adjacent (Key : Key_Type) return Key_Array is
        (case Key is when '0' => "2A", when '1' => "24", when '2' => "0135",
           when '3' => "26A", when '4' => "157", when '5' => "2468",
           when '6' => "359", when '7' => "48", when '8' => "579",
           when '9' => "68", when 'A' => "03");

      function Adjacent (From, To : Key_Type) return Boolean is
        (for some Key of Adjacent (From) => Key = To);

      function Distance (From, To : Key_Type) return Natural is
        (case From is
           when '0' =>
             (case To is when '0' => 0, when '1' => 2, when '2' => 1,
                when '3' => 2, when '4' => 3, when '5' => 2, when '6' => 3,
                when '7' => 4, when '8' => 3, when '9' => 4, when 'A' => 1),
           when '1' =>
             (case To is when '0' .. '0' => Distance (From => To, To => From),
                when '1' => 0, when '2' => 1, when '3' => 2, when '4' => 1,
                when '5' => 2, when '6' => 3, when '7' => 2, when '8' => 3,
                when '9' => 4, when 'A' => 3),
           when '2' =>
             (case To is when '0' .. '1' => Distance (From => To, To => From),
                when '2' => 0, when '3' => 1, when '4' => 2, when '5' => 1,
                when '6' => 2, when '7' => 3, when '8' => 2, when '9' => 3,
                when 'A' => 2),
           when '3' =>
             (case To is when '0' .. '2' => Distance (From => To, To => From),
                when '3' => 0, when '4' => 3, when '5' => 2, when '6' => 1,
                when '7' => 4, when '8' => 3, when '9' => 2, when 'A' => 1),
           when '4' =>
             (case To is when '0' .. '3' => Distance (From => To, To => From),
                when '4' => 0, when '5' => 1, when '6' => 2, when '7' => 1,
                when '8' => 2, when '9' => 3, when 'A' => 4),
           when '5' =>
             (case To is when '0' .. '4' => Distance (From => To, To => From),
                when '5' => 0, when '6' => 1, when '7' => 2, when '8' => 1,
                when '9' => 2, when 'A' => 3),
           when '6' =>
             (case To is when '0' .. '5' => Distance (From => To, To => From),
                when '6' => 0, when '7' => 3, when '8' => 2, when '9' => 1,
                when 'A' => 2),
           when '7' =>
             (case To is when '0' .. '6' => Distance (From => To, To => From),
                when '7' => 0, when '8' => 1, when '9' => 2, when 'A' => 5),
           when '8' =>
             (case To is when '0' .. '7' => Distance (From => To, To => From),
                when '8' => 0, when '9' => 1, when 'A' => 4),
           when '9' =>
             (case To is when '0' .. '8' => Distance (From => To, To => From),
                when '9' => 0, when 'A' => 3),
           when 'A' =>
             (case To is when '0' .. '9' => Distance (From => To, To => From),
                when 'A' => 0));

      function Get_Code (File : File_Type) return Code_Type;

      function To_Character (Key : Key_Type) return Character is
        (case Key is
           when '0' .. '9' =>
             Character'Val (Key_Type'Pos (Key) + Character'Pos ('0')),
           when 'A' => 'A');

      function To_Bounded (Keys : Key_Array) return Bounded_Key_Array;

      function To_Number (Code : Code_Type) return Natural with
        Post => To_Number'Result < 1_000;

      function To_String (Keys : Key_Array) return String;
   end Numeric;

   --      +---+---+
   --      | ^ | A |
   --  +---+---+---+
   --  | < | v | > |
   --  +---+---+---+
   package Directional is
      type Key_Type is ('A', 'v', '<', '>', '^');
      type Key_Set is array (Key_Type) of Boolean;
      type Key_Array is array (Positive range <>) of Key_Type;
      type Bounded_Key_Array is record
         Length   : Natural;
         Elements : Key_Array (1 .. 32);
      end record;
      type Key_Array_List is array (Positive range <>) of Bounded_Key_Array;

      function Adjacent (Key : Key_Type) return Key_Array is
        (case Key is when 'A' => ">^", when 'v' => "<>^", when '<' => "v",
           when '>' => "Av", when '^' => "Av");

      function Adjacent (From, To : Key_Type) return Boolean is
        (for some Key of Adjacent (From) => Key = To);

      function Distance (From, To : Key_Type) return Natural is
        (case From is
           when 'A' =>
             (case To is when 'A' => 0, when 'v' => 2, when '<' => 3,
                when '>' => 1, when '^' => 1),
           when 'v' =>
             (case To is when 'v' => 0, when '<' => 1, when '>' => 1,
                when '^' => 1,
                when 'A' .. 'A' => Distance (From => To, To => From)),
           when '<' =>
             (case To is when '<' => 0, when '>' => 2, when '^' => 2,
                when 'A' .. 'v' => Distance (From => To, To => From)),
           when '>' =>
             (case To is when '>' => 0, when '^' => 2,
                when 'A' .. '<' => Distance (From => To, To => From)),
           when '^' =>
             (case To is when '^' => 0,
                when 'A' .. '>' => Distance (From => To, To => From)));

      function Revert (Key : Key_Type) return Key_Type is
        (case Key is when 'v' => '^', when '<' => '>', when '>' => '<',
           when '^' => 'v', when 'A' => 'A');

      function Revert (Keys : Key_Array) return Key_Array with
        Post => Revert'Result'Length = Keys'Length;

      function To_Character (Key : Key_Type) return Character is
        (case Key is when 'A' => 'A', when 'v' => 'v', when '<' => '<',
           when '>' => '>', when '^' => '^');

      function To_Bounded (Keys : Key_Array) return Bounded_Key_Array;

      function To_String (Keys : Key_Array) return String;
   end Directional;

   --  Button presses needed on the corresponding directional keypad
   --  to move from one button to another on the numeric keypad.
   function Translate
     (From, To : Numeric.Key_Type) return Directional.Key_Array with
     Post => Translate'Result'Length <= 5;
   function Translate (Keys : Numeric.Key_Array) return Directional.Key_Array;
   function Translate
     (Keys : Numeric.Key_Array; Current : in out Numeric.Key_Type)
      return Directional.Key_Array;

   --  Button presses needed on the corresponding directional keypad
   --  to move on the second order directional keypad.
   function Translate
     (From, To : Directional.Key_Type) return Directional.Key_Array with
     Post => Translate'Result'Length <= 3;
   function Translate
     (Keys : Directional.Key_Array) return Directional.Key_Array;
   function Translate
     (Keys : Directional.Key_Array; Current : in out Directional.Key_Type)
      return Directional.Key_Array;
end Advent.Day_21;
