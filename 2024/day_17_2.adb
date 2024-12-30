with Ada.Text_IO;    use Ada.Text_IO;
with Advent.Day_17;  use Advent.Day_17;
with Advent.Debug;   use Advent.Debug;
with Advent.Ternary; use Advent.Ternary;
with Advent;         use Advent;

procedure Day_17_2 is
   Size : constant := 63;

   subtype Ambiguous_Register is Ternary_Array (0 .. Size - 1);
   type Ambiguous_Register_Array is
     array (Positive range <>) of Ambiguous_Register;

   function To_Register is new To_Modular (Register);

   function From_String (S : String) return Ambiguous_Register is
      R : constant Ambiguous_Register := To_Ternary_Array (S, Size, Unknown);
   begin
      if Debug_Enabled then
         Put_Line
           (Standard_Error,
            "String " & S'Image & " => " & To_String (R)'Image);
      end if;
      return R;
   end From_String;

   function From_Number
     (N : Number; I : Natural := 0) return Ambiguous_Register
   is
      R : Ambiguous_Register := [others => Unknown];
   begin
      R (I)     := (if (N and 2#001#) /= 0 then True else False);
      R (I + 1) := (if (N and 2#010#) /= 0 then True else False);
      R (I + 2) := (if (N and 2#100#) /= 0 then True else False);

      if Debug_Enabled then
         Put_Line
           (Standard_Error,
            "Number    " & N'Image & " => " & To_String (R)'Image);
      end if;

      return R;
   end From_Number;

   --  XXX: Only for specific puzzle input.
   function Possible_Patterns
     (Output : Number; I : Natural := 0) return Ambiguous_Register_Array
   is
      Buffer : Ambiguous_Register_Array (1 .. 8);
      Offset : Positive := Buffer'First;
   begin
      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("000"), I, Unknown),
           From_Number (2#000# xor Output, I + 4));
      Offset          := Offset + 1;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("001"), I, Unknown),
           From_Number (2#001# xor Output, I + 5));
      Offset          := Offset + 1;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("010"), I, Unknown),
           From_Number (2#010# xor Output, I + 6));
      Offset          := Offset + 1;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("011"), I, Unknown),
           From_Number (2#011# xor Output, I + 7));
      Offset          := Offset + 1;

      begin
         Buffer (Offset) :=
           Unify
             (Shift_Left (From_String ("100"), I, Unknown),
              From_Number (2#100# xor Output, I + 0));
         Offset          := Offset + 1;
      exception
         when Not_Unifiable_Error =>
            Put_Line (Standard_Error, "Not_Unifiable_Error");
      end;

      begin
         Buffer (Offset) :=
           Unify
             (Shift_Left (From_String ("101"), I, Unknown),
              From_Number (2#101# xor Output, I + 1));
         Offset          := Offset + 1;
      exception
         when Not_Unifiable_Error =>
            Put_Line (Standard_Error, "Not_Unifiable_Error");
      end;

      begin
         Buffer (Offset) :=
           Unify
             (Shift_Left (From_String ("110"), I, Unknown),
              From_Number (2#110# xor Output, I + 2));
         Offset          := Offset + 1;
      exception
         when Not_Unifiable_Error =>
            Put_Line (Standard_Error, "Not_Unifiable_Error");
      end;

      Buffer (Offset) :=
        Unify
          (Shift_Left (From_String ("111"), I, Unknown),
           From_Number (2#111# xor Output, I + 3));
      Offset          := Offset + 1;

      return Buffer (1 .. Offset - 1);
   end Possible_Patterns;

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

   function Possible_Patterns
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
                   Possible_Patterns (Outputs (Outputs'First + I), I * 3)));
      end Iterate;
   begin
      return
        Iterate (1, Possible_Patterns (Outputs (Outputs'First + 0), 0 * 3));
   end Possible_Patterns;

   Unused_CPU : constant CPU_Type     := Get_CPU (Standard_Input);
   Program    : constant Number_Array := Get_Program (Standard_Input);
   A, Min_A   : Register              := Register'Last;
begin
   Put_Line (Standard_Error, "Possible_Patterns:");

   for P of Possible_Patterns (Program) loop
      Put_Line (Standard_Error, "Pattern  " & To_String (P)'Image);
      Put_Line (Standard_Error, "Trim     " & To_String (Trim (P))'Image);
      Put_Line
        (Standard_Error,
         "Disambig " & To_String (Disambiguate (Trim (P), False))'Image);
      begin
         A     := To_Register (Disambiguate (Trim (P), False));
         --  FIXME: The minimum value doesn't output the last 0 in the program.
         Min_A := Register'Min (Min_A, A);
         Put_Line (Standard_Error, "A " & A'Image & ", Min_A " & Min_A'Image);
      exception
         when Not_Unifiable_Error =>
            null;
      end;
      New_Line (Standard_Error);
   end loop;

   --  TODO
end Day_17_2;
