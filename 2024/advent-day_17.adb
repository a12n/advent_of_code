package body Advent.Day_17 is
   package Register_Text_IO is new Ada.Text_IO.Modular_IO (Register);

   function From_String (Chars : String) return Number_Array is
      Numbers : Number_Array (1 .. (Chars'Length + 1) / 2) := [others => 0];
   begin
      for I in Numbers'Range loop
         --  TODO: Check ','
         Numbers (I) :=
           Character'Pos (Chars (Chars'First + 2 * I - 2)) -
           Character'Pos ('0');
      end loop;
      return Numbers;
   end From_String;

   function Get_CPU (File : File_Type) return CPU_Type is
      function Get_Register
        (File : File_Type; Name : Register_Name) return Register
      is
         use Register_Text_IO;
         Line   : constant String := Get_Line (File);
         R      : Register;
         Unused : Positive;
      begin
         if Line (1 .. 9) /= "Register " or Line (10 .. 10) /= Name'Image or
           Line (11 .. 12) /= ": "
         then
            raise Constraint_Error;
         end if;
         Get (Line (13 .. Line'Last), R, Unused);
         return R;
      end Get_Register;

      CPU : CPU_Type;
   begin
      CPU.R (A) := Get_Register (File, A);
      CPU.R (B) := Get_Register (File, B);
      CPU.R (C) := Get_Register (File, C);

      Skip_Line (File);

      return CPU;
   end Get_CPU;

   function Get_Program (File : File_Type) return Number_Array is
      Line : constant String := Get_Line (File);
   begin
      if Line (1 .. 9) /= "Program: " then
         raise Constraint_Error;
      end if;
      return From_String (Line (10 .. Line'Last));
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
