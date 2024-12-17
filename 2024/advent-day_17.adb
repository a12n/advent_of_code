package body Advent.Day_17 is
   function Get_CPU (File : File_Type) return CPU_Type is
      package Register_Text_IO is new Ada.Text_IO.Integer_IO (Register);
      use Register_Text_IO;

      A_Line : String := Get_Line (File);
      B_Line : String := Get_Line (File);
      C_Line : String := Get_Line (File);
      Unused : Positive;
      CPU    : CPU_Type;

      procedure Filter (Chars : in out String) is
      begin
         for I in Chars'Range loop
            if Chars (I) not in '0' .. '9' then
               Chars (I) := ' ';
            end if;
         end loop;
      end Filter;
   begin
      if A_Line (1 .. 12) /= "Register A: " or
        B_Line (1 .. 12) /= "Register B: " or
        C_Line (1 .. 12) /= "Register C: "
      then
         raise Constraint_Error;
      end if;
      Filter (A_Line);
      Filter (B_Line);
      Filter (C_Line);
      Get (A_Line, CPU.A, Unused);
      Get (B_Line, CPU.B, Unused);
      Get (C_Line, CPU.C, Unused);
      return CPU;
   end Get_CPU;

   function Get_Program (File : File_Type) return Number_Array is
   begin
      --  TODO
      return Number_Array'[];
   end Get_Program;

   function Run
     (CPU : in out CPU_Type; Program : Number_Array) return Number_Array
   is
   begin
      --  TODO
      return Number_Array'[4, 6, 3, 5, 6, 3, 5, 2, 1, 0];
   end Run;

   function To_String (Numbers : Number_Array) return String is
      Chars : String (1 .. 2 * Numbers'Length - 1) := [others => ','];
   begin
      for I in Numbers'Range loop
         Chars (2 * I - 1) :=
           Character'Val (Character'Pos ('0') + Numbers (I));
      end loop;
      return Chars;
   end To_String;
end Advent.Day_17;
