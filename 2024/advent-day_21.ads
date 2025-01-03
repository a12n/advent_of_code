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
      type Key_Array is array (Positive range <>) of Key_Type;

      subtype Digit_Key_Type is Key_Type range '0' .. '9';
      subtype Code_Type is Key_Array (1 .. 4) with
          Dynamic_Predicate =>
           (for all I in 1 .. 3 => Code_Type (I) in '0' .. '9') and
           Code_Type (4) = 'A';

      function Adjacent_Keys (Key : Key_Type) return Key_Array is
        (case Key is when '0' => "2A", when '1' => "24", when '2' => "0135",
           when '3' => "26A", when '4' => "157", when '5' => "2468",
           when '6' => "359", when '7' => "48", when '8' => "579",
           when '9' => "68", when 'A' => "03");

      function Adjacent (From, To : Key_Type) return Boolean is
        (for some Key of Adjacent_Keys (From) => Key = To);

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

      function Distance (Keys : Key_Array) return Natural;

      function Get_Code (File : File_Type) return Code_Type;

      function To_Character (Key : Key_Type) return Character is
        (case Key is
           when '0' .. '9' =>
             Character'Val (Key_Type'Pos (Key) + Character'Pos ('0')),
           when 'A' => 'A');

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
      type Key_Type is ('v', '<', '>', '^', 'A');
      type Key_Array is array (Positive range <>) of Key_Type;

      subtype Move_Key_Type is Key_Type range 'v' .. '^';

      function Adjacent_Keys (Key : Key_Type) return Key_Array is
        (case Key is when 'v' => "<>^", when '<' => "v", when '>' => "vA",
           when '^' => "vA", when 'A' => ">^");

      function Adjacent (From, To : Key_Type) return Boolean is
        (for some Key of Adjacent_Keys (From) => Key = To);

      function Distance (From, To : Key_Type) return Natural is
        (case From is
           when 'v' =>
             (case To is when 'v' => 0, when '<' => 1, when '>' => 1,
                when '^' => 1, when 'A' => 2),
           when '<' =>
             (case To is when 'v' .. 'v' => Distance (From => To, To => From),
                when '<' => 0, when '>' => 2, when '^' => 2, when 'A' => 3),
           when '>' =>
             (case To is when 'v' .. '<' => Distance (From => To, To => From),
                when '>' => 0, when '^' => 2, when 'A' => 1),
           when '^' =>
             (case To is when 'v' .. '>' => Distance (From => To, To => From),
                when '^' => 0, when 'A' => 1),
           when 'A' =>
             (case To is when 'v' .. '^' => Distance (From => To, To => From),
                when 'A' => 0));

      function Distance (Keys : Key_Array) return Natural;

      function To_Character (Key : Key_Type) return Character is
        (case Key is when 'v' => 'v', when '<' => '<', when '>' => '>',
           when '^' => '^', when 'A' => 'A');

      function To_String (Keys : Key_Array) return String;
   end Directional;

   Keypad_Range_Error : exception;

   function Move
     (Key : Numeric.Key_Type; Dir : Directional.Move_Key_Type)
      return Numeric.Key_Type is
     (case Dir is
        when 'v' =>
          (case Key is when '2' => '0', when '3' => 'A', when '4' => '1',
             when '5' => '2', when '6' => '3', when '7' => '4',
             when '8' => '5', when '9' => '6',
             when others => raise Keypad_Range_Error),
        when '<' =>
          (case Key is when '2' => '1', when '3' => '2', when '5' => '4',
             when '6' => '5', when '8' => '7', when '9' => '8',
             when 'A' => '0', when others => raise Keypad_Range_Error),
        when '>' =>
          (case Key is when '0' => 'A', when '1' => '2', when '2' => '3',
             when '4' => '5', when '5' => '6', when '7' => '8',
             when '8' => '9', when others => raise Keypad_Range_Error),
        when '^' =>
          (case Key is when '0' => '2', when '1' => '4', when '2' => '5',
             when '3' => '6', when '4' => '7', when '5' => '8',
             when '6' => '9', when 'A' => '3',
             when others => raise Keypad_Range_Error));

   function Move
     (Key : Directional.Key_Type; Dir : Directional.Move_Key_Type)
      return Directional.Key_Type is
     (case Dir is
        when 'v' =>
          (case Key is when '^' => 'v', when 'A' => '>',
             when others => raise Keypad_Range_Error),
        when '<' =>
          (case Key is when 'v' => '<', when '>' => 'v', when 'A' => '^',
             when others => raise Keypad_Range_Error),
        when '>' =>
          (case Key is when 'v' => '>', when '<' => 'v', when '^' => 'A',
             when others => raise Keypad_Range_Error),
        when '^' =>
          (case Key is when 'v' => '^', when '>' => 'A',
             when others => raise Keypad_Range_Error));

   --  Button presses needed on the corresponding directional keypad
   --  to move from one button to another on the numeric keypad.
   --
   --  The table is generated with minimum distance paths from Move
   --  function.
   function Translate
     (From, To : Numeric.Key_Type) return Directional.Key_Array is
     (case From is
        when '0' =>
          (case To is when '0' => "", when '1' => "^<", when '2' => "^",
             when '3' => "^>", when '4' => "^^<", when '5' => "^^",
             when '6' => "^^>", when '7' => "^^^<", when '8' => "^^^",
             when '9' => "^^^>", when 'A' => ">"),
        when '1' =>
          (case To is when '0' => ">v", when '1' => "", when '2' => ">",
             when '3' => ">>", when '4' => "^", when '5' => "^>",
             when '6' => "^>>", when '7' => "^^", when '8' => "^^>",
             when '9' => "^^>>", when 'A' => ">>v"),
        when '2' =>
          (case To is when '0' => "v", when '1' => "<", when '2' => "",
             when '3' => ">", when '4' => "<^", when '5' => "^",
             when '6' => "^>", when '7' => "<^^", when '8' => "^^",
             when '9' => "^^>", when 'A' => "v>"),
        when '3' =>
          (case To is when '0' => "<v", when '1' => "<<", when '2' => "<",
             when '3' => "", when '4' => "<<^", when '5' => "<^",
             when '6' => "^", when '7' => "<<^^", when '8' => "<^^",
             when '9' => "^^", when 'A' => "v"),
        when '4' =>
          (case To is when '0' => ">vv", when '1' => "v", when '2' => "v>",
             when '3' => "v>>", when '4' => "", when '5' => ">",
             when '6' => ">>", when '7' => "^", when '8' => "^>",
             when '9' => "^>>", when 'A' => ">>vv"),
        when '5' =>
          (case To is when '0' => "vv", when '1' => "<v", when '2' => "v",
             when '3' => "v>", when '4' => "<", when '5' => "",
             when '6' => ">", when '7' => "<^", when '8' => "^",
             when '9' => "^>", when 'A' => "vv>"),
        when '6' =>
          (case To is when '0' => "<vv", when '1' => "<<v", when '2' => "<v",
             when '3' => "v", when '4' => "<<", when '5' => "<",
             when '6' => "", when '7' => "<<^", when '8' => "<^",
             when '9' => "^", when 'A' => "vv"),
        when '7' =>
          (case To is when '0' => ">vvv", when '1' => "vv", when '2' => "vv>",
             when '3' => "vv>>", when '4' => "v", when '5' => "v>",
             when '6' => "v>>", when '7' => "", when '8' => ">",
             when '9' => ">>", when 'A' => ">>vvv"),
        when '8' =>
          (case To is when '0' => "vvv", when '1' => "<vv", when '2' => "vv",
             when '3' => "vv>", when '4' => "<v", when '5' => "v",
             when '6' => "v>", when '7' => "<", when '8' => "",
             when '9' => ">", when 'A' => "vvv>"),
        when '9' =>
          (case To is when '0' => "<vvv", when '1' => "<<vv",
             when '2' => "<vv", when '3' => "vv", when '4' => "<<v",
             when '5' => "<v", when '6' => "v", when '7' => "<<",
             when '8' => "<", when '9' => "", when 'A' => "vvv"),
        when 'A' =>
          (case To is when '0' => "<", when '1' => "^<<", when '2' => "<^",
             when '3' => "^", when '4' => "^^<<", when '5' => "<^^",
             when '6' => "^^", when '7' => "^^^<<", when '8' => "<^^^",
             when '9' => "^^^", when 'A' => ""));

   --  Button presses needed on the corresponding directional keypad
   --  to move on the second order directional keypad.
   function Translate
     (From, To : Directional.Key_Type) return Directional.Key_Array is
     (case From is
        when 'v' =>
          (case To is when 'v' => "", when '<' => "<", when '>' => ">",
             when '^' => "^", when 'A' => "^>"),
        when '<' =>
          (case To is when 'v' => ">", when '<' => "", when '>' => ">>",
             when '^' => ">^", when 'A' => ">>^"),
        when '>' =>
          (case To is when 'v' => "<", when '<' => "<<", when '>' => "",
             when '^' => "<^", when 'A' => "^"),
        when '^' =>
          (case To is when 'v' => "v", when '<' => "v<", when '>' => "v>",
             when '^' => "", when 'A' => ">"),
        when 'A' =>
          (case To is when 'v' => "<v", when '<' => "v<<", when '>' => "v",
             when '^' => "<", when 'A' => ""));

   function Translate (Keys : Numeric.Key_Array) return Directional.Key_Array;

   function Translate
     (Keys : Directional.Key_Array) return Directional.Key_Array;

   type Count_Type is range 0 .. 2**64;

   package Count_Text_IO is new Ada.Text_IO.Integer_IO (Count_Type);

   type Translator_Type (Max_Times : Positive) is limited private;

   function Translate_Length
     (Translator : Translator_Type; Keys : Directional.Key_Array;
      Times      : Positive := 1) return Count_Type with
     Pre => (Times <= Translator.Max_Times);

   procedure Main (File : File_Type; Times : Natural);

private
   type Translate_Table is
     array
       (Positive range <>, Directional.Key_Type range <>,
        Directional.Key_Type range <>) of Count_Type;

   function Make_Table (Max_Times : Positive) return Translate_Table;

   type Translator_Type (Max_Times : Positive) is record
      Table : Translate_Table
        (1 .. Max_Times, Directional.Key_Type'Range,
         Directional.Key_Type'Range) :=
        Make_Table (Max_Times);
   end record;
end Advent.Day_21;
