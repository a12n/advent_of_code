package body Advent.Ternary is
   function Array_Binary_Operator (A, B : Ternary_Array) return Ternary_Array
   is
   begin
      return R : Ternary_Array (0 .. A'Length - 1) do
         for I in R'Range loop
            R (I) := Operator (A (A'First + I), B (B'First + I));
         end loop;
      end return;
   end Array_Binary_Operator;

   function Array_Unary_Operator (A : Ternary_Array) return Ternary_Array is
   begin
      return R : Ternary_Array (0 .. A'Length - 1) do
         for I in R'Range loop
            R (I) := Operator (A (A'First + I));
         end loop;
      end return;
   end Array_Unary_Operator;

   function AND_Array_Operator is new Array_Binary_Operator ("and");
   function NOT_Array_Operator is new Array_Unary_Operator ("not");
   function OR_Array_Operator is new Array_Binary_Operator ("or");
   function Unify_Array is new Array_Binary_Operator (Unify);
   function XOR_Array_Operator is new Array_Binary_Operator ("xor");

   function "and" (A, B : Ternary_Array) return Ternary_Array renames
     AND_Array_Operator;
   function "not" (A : Ternary_Array) return Ternary_Array renames
     NOT_Array_Operator;
   function "or" (A, B : Ternary_Array) return Ternary_Array renames
     OR_Array_Operator;
   function "xor" (A, B : Ternary_Array) return Ternary_Array renames
     XOR_Array_Operator;
   function Unify (A, B : Ternary_Array) return Ternary_Array renames
     Unify_Array;

   function Shift_Left
     (A : Ternary_Array; N : Natural; Shift_In : Ternary := False)
      return Ternary_Array
   is
   begin
      return R : Ternary_Array (0 .. A'Length - 1) do
         if N = 0 then
            R := A (A'First .. A'Last);
         elsif N >= A'Length then
            R := [others => Shift_In];
         else
            --  Least significant at index 'First (0), most
            --  significant at index 'Last.
            R (R'First + N .. R'Last)      := A (A'First .. A'Last - N);
            R (R'First .. R'First + N - 1) := [others => Shift_In];
         end if;
      end return;
   end Shift_Left;

   function Shift_Right
     (A : Ternary_Array; N : Natural; Shift_In : Ternary := False)
      return Ternary_Array
   is
   begin
      return R : Ternary_Array (0 .. A'Length - 1) do
         if N = 0 then
            R := A;
         elsif N >= A'Length then
            R := [others => Shift_In];
         else
            R (R'First .. R'Last - N)    := A (A'First + N .. A'Last);
            R (R'Last - N + 1 .. R'Last) := [others => Shift_In];
         end if;
      end return;
   end Shift_Right;

   function To_Modular (A : Ternary_Array) return Modular_Type is
      N : Modular_Type := 0;
      K : Modular_Type := 1;            --  2**0
   begin
      for I in A'Range loop
         case A (I) is
            when False =>
               null;
            when True =>
               N := N or K;
            when Unknown =>
               raise Constraint_Error;
         end case;
         K := K * 2;
      end loop;
      return N;
   end To_Modular;

   function To_String (A : Ternary_Array) return String is
   begin
      return S : String (1 .. A'Length) do
         for I in 0 .. A'Length - 1 loop
            S (S'Last - I) := To_Character (A (A'First + I));
         end loop;
      end return;
   end To_String;

   function To_Ternary_Array (S : String) return Ternary_Array is
   begin
      return To_Ternary_Array (S, S'Length);
   end To_Ternary_Array;

   function To_Ternary_Array
     (S : String; N : Positive; Shift_In : Ternary := False)
      return Ternary_Array
   is
   begin
      return A : Ternary_Array (0 .. N - 1) := [others => Shift_In] do
         for I in 0 .. S'Length - 1 loop
            A (A'First + I) := To_Ternary (S (S'Last - I));
         end loop;
      end return;
   end To_Ternary_Array;

   function Trim (A : Ternary_Array) return Ternary_Array is
      Last : Natural := A'Last;
   begin
      while A (Last) = Unknown loop
         Last := Last - 1;
      end loop;
      return A (A'First .. Last);
   end Trim;
end Advent.Ternary;
