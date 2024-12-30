package Advent.Ternary is
   pragma Pure;

   type Ternary is (False, True, Unknown);

   --  Least significant digit is at 'First, and the most significant
   --  one at 'Last.
   type Ternary_Array is array (Natural range <>) of Ternary;
   pragma Pack (Ternary_Array);

   function "and" (A, B : Ternary) return Ternary;
   function "not" (A : Ternary) return Ternary;
   function "or" (A, B : Ternary) return Ternary;
   function "xor" (A, B : Ternary) return Ternary;

   function "and" (A, B : Ternary_Array) return Ternary_Array;
   function "not" (A : Ternary_Array) return Ternary_Array;
   function "or" (A, B : Ternary_Array) return Ternary_Array;
   function "xor" (A, B : Ternary_Array) return Ternary_Array;

   function Shift_Left
     (A : Ternary_Array; N : Natural; Shift_In : Ternary := False)
      return Ternary_Array;

   function Shift_Right
     (A : Ternary_Array; N : Natural; Shift_In : Ternary := False)
      return Ternary_Array;

   function To_Character (A : Ternary) return Character is
     (case A is when False => '0', when True => '1', when Unknown => '_');

   generic
      type Modular_Type is mod <>;
   function To_Modular (A : Ternary_Array) return Modular_Type with
     Pre => Modular_Type'Size >= A'Length;

   function To_String (A : Ternary_Array) return String;

   function To_Ternary (C : Character) return Ternary is
     (case C is when '0' | 'F' | '.' => False, when '1' | 'T' | '#' => True,
        when '?' | '-' | '_' => Unknown,
        when others => raise Constraint_Error);

   function To_Ternary_Array (S : String) return Ternary_Array;

   function To_Ternary_Array
     (S : String; N : Positive; Shift_In : Ternary := False)
      return Ternary_Array with
     Pre => N >= S'Length;

   --  Remove any leading Unknown digits from the most significant
   --  digit side.
   function Trim (A : Ternary_Array) return Ternary_Array;

   Not_Unifiable_Error : exception;

   function Unify (A, B : Ternary) return Ternary is
     (case A is
        when False =>
          (case B is when False | Unknown => False,
             when True => raise Not_Unifiable_Error),
        when True =>
          (case B is when False => raise Not_Unifiable_Error,
             when True | Unknown => True),
        when Unknown =>
          (case B is when False => False, when True => True,
             when Unknown => Unknown));
   function Unify (A, B : Ternary_Array) return Ternary_Array;

   pragma Inline ("and", "not", "or", "xor", To_Character);

private
   AND_Table : constant array (Ternary, Ternary) of Ternary :=
     [False  => [others => False],
     True    => [False => False, True => True, Unknown => Unknown],
     Unknown => [False => False, True => Unknown, Unknown => Unknown]];

   NOT_Table : constant array (Ternary) of Ternary :=
     [False => True, True => False, Unknown => Unknown];

   OR_Table : constant array (Ternary, Ternary) of Ternary :=
     [False  => [False => False, True => True, Unknown => Unknown],
     True    => [others => True],
     Unknown => [False => Unknown, True => True, Unknown => Unknown]];

   XOR_Table : constant array (Ternary, Ternary) of Ternary :=
     [False  => [False => False, True => True, Unknown => Unknown],
     True    => [False => True, True => False, Unknown => Unknown],
     Unknown => [others => Unknown]];

   function "and" (A, B : Ternary) return Ternary is (AND_Table (A, B));
   function "not" (A : Ternary) return Ternary is (NOT_Table (A));
   function "or" (A, B : Ternary) return Ternary is (OR_Table (A, B));
   function "xor" (A, B : Ternary) return Ternary is (XOR_Table (A, B));

   generic
      with function Operator (A, B : Ternary) return Ternary;
   function Array_Binary_Operator
     (A, B : Ternary_Array) return Ternary_Array with
     Pre => A'Length = B'Length;

   generic
      with function Operator (A : Ternary) return Ternary;
   function Array_Unary_Operator (A : Ternary_Array) return Ternary_Array;
end Advent.Ternary;
