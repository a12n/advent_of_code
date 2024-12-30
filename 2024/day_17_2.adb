with Ada.Text_IO;    use Ada.Text_IO;
with Advent.Day_17;  use Advent.Day_17;
with Advent.Ternary; use Advent.Ternary;
with Advent;         use Advent;

procedure Day_17_2 is
   subtype Ambiguous_Register is Ternary_Array (0 .. Register'Size - 1);
   type Ambiguous_Register_Array is
     array (Positive range <>) of Ambiguous_Register;

   --  Convert to/from Register type.
   function To_Register is new Generic_To_Modular (Register);

   --  Convert from 3 bit Number type.
   function From_Number (N : Number) return Ambiguous_Register is
   begin
      return
        Ambiguous_Register'
          [0     => (if (N and 2#001#) /= 0 then True else False),
          1      => (if (N and 2#010#) /= 0 then True else False),
          2      => (if (N and 2#100#) /= 0 then True else False),
          others => Unknown];
   end From_Number;

   --  Convert from String with Unknown values shifted-in.
   function From_String (S : String) return Ambiguous_Register is
     (To_Ternary_Array (S, Register'Size, Unknown));

   --  XXX: Only for specific puzzle input.
   function Register_Patterns
     (Output : Number; I : Natural := 0) return Ambiguous_Register_Array
   is
      Buffer : Ambiguous_Register_Array (1 .. 8);
      Offset : Positive := Buffer'First;
   begin
      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("000"), I, Unknown),
           Shift_Left (From_Number (2#000# xor Output), I + 4, Unknown));
      Offset          := Offset + 1;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("001"), I, Unknown),
           Shift_Left (From_Number (2#001# xor Output), I + 5, Unknown));
      Offset          := Offset + 1;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("010"), I, Unknown),
           Shift_Left (From_Number (2#010# xor Output), I + 6, Unknown));
      Offset          := Offset + 1;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("011"), I, Unknown),
           Shift_Left (From_Number (2#011# xor Output), I + 7, Unknown));
      Offset          := Offset + 1;

      begin
         Buffer (Offset) :=
           Unify
             (Shift_Left (From_String ("100"), I, Unknown),
              Shift_Left (From_Number (2#100# xor Output), I + 0, Unknown));
         Offset          := Offset + 1;
      exception
         when Not_Unifiable_Error =>
            null;
      end;

      begin
         Buffer (Offset) :=
           Unify
             (Shift_Left (From_String ("101"), I, Unknown),
              Shift_Left (From_Number (2#101# xor Output), I + 1, Unknown));
         Offset          := Offset + 1;
      exception
         when Not_Unifiable_Error =>
            null;
      end;

      begin
         Buffer (Offset) :=
           Unify
             (Shift_Left (From_String ("110"), I, Unknown),
              Shift_Left (From_Number (2#110# xor Output), I + 2, Unknown));
         Offset          := Offset + 1;
      exception
         when Not_Unifiable_Error =>
            null;
      end;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("111"), I, Unknown),
           Shift_Left (From_Number (2#111# xor Output), I + 3, Unknown));
      Offset          := Offset + 1;

      return Buffer (1 .. Offset - 1);
   end Register_Patterns;

   function Unify
     (A, B : Ambiguous_Register_Array) return Ambiguous_Register_Array
   is
      Buffer : Ambiguous_Register_Array (1 .. A'Length * B'Length);
      Offset : Positive := Buffer'First;
   begin
      for I in A'Range loop
         for J in B'Range loop
            begin
               Buffer (Offset) := Unify (A (I), B (J));
               Offset          := Offset + 1;
            exception
               when Not_Unifiable_Error =>
                  null;
            end;
         end loop;
      end loop;
      return Buffer (1 .. Offset - 1);
   end Unify;

   function Register_Patterns
     (Outputs : Number_Array) return Ambiguous_Register_Array
   is
      function Iterate
        (I : Natural; Result : Ambiguous_Register_Array)
         return Ambiguous_Register_Array
      is
      begin
         if I = Outputs'Length then
            return Result;
         end if;
         return
           Iterate
             (I + 1,
              Unify
                (Result,
                   Register_Patterns (Outputs (Outputs'First + I), I * 3)));
      end Iterate;
   begin
      return
        Iterate (1, Register_Patterns (Outputs (Outputs'First + 0), 0 * 3));
   end Register_Patterns;

   CPU     : constant CPU_Type     := Get_CPU (Standard_Input);
   Program : constant Number_Array := Get_Program (Standard_Input);

   function Quine (Initial : Register) return Boolean is
      Temp_CPU : CPU_Type := CPU;
      Output   : Number;
   begin
      Temp_CPU.R (A) := Initial;
      --  If program outputs itself instructions…
      for I in Program'Range loop
         if Temp_CPU.Run (Program, Output) then
            if Output /= Program (I) then
               return False;
            end if;
         else
            return False;
         end if;
      end loop;
      --  …and then halts.
      return not Temp_CPU.Run (Program, Output);
   end Quine;

   Min_A : Register := Register'Last;

   use Register_Text_IO;
begin
   for Pattern of Register_Patterns (Program) loop
      begin
         declare
            Initial_A : constant Register :=
              To_Register (Disambiguate (Trim (Pattern), False));
         begin
            if Quine (Initial_A) and Initial_A < Min_A then
               Min_A := Initial_A;
            end if;
         end;
      exception
         when Not_Unifiable_Error =>
            null;
      end;
   end loop;

   Put (Min_A, 0);
   New_Line;
end Day_17_2;
