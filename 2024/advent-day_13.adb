package body Advent.Day_13 is
   function Input_Entry (File : File_Type) return Claw_Machine is
      package Offset_Text_IO is new Ada.Text_IO.Integer_IO (Offset);
      package Position_Text_IO is new Ada.Text_IO.Integer_IO (Position);

      use Offset_Text_IO;
      use Position_Text_IO;

      A_Line     : String := Get_Line (File);
      B_Line     : String := Get_Line (File);
      Prize_Line : String := Get_Line (File);

      Start   : Positive := 1;
      Machine : Claw_Machine;

      procedure Filter_Digits (Line : in out String) is
      begin
         for Char of Line loop
            if Char not in '0' .. '9' then
               Char := ' ';
            end if;
         end loop;
      end Filter_Digits;
   begin
      if A_Line (1 .. 12) /= "Button A: X+" or
        B_Line (1 .. 12) /= "Button B: X+" or
        Prize_Line (1 .. 9) /= "Prize: X="
      then
         raise Constraint_Error;
      end if;

      --  Skip next empty line.
      begin
         Skip_Line (File);
      exception
         when End_Error =>
            null;
      end;

      Filter_Digits (A_Line);
      Filter_Digits (B_Line);
      Filter_Digits (Prize_Line);

      Get (A_Line, Machine.A.X, Start);
      Get (A_Line (Start + 1 .. A_Line'Last), Machine.A.Y, Start);
      Get (B_Line, Machine.B.X, Start);
      Get (B_Line (Start + 1 .. B_Line'Last), Machine.B.Y, Start);
      Get (Prize_Line, Machine.Prize.X, Start);
      Get (Prize_Line (Start + 1 .. Prize_Line'Last), Machine.Prize.Y, Start);

      return Machine;
   end Input_Entry;

   function Solution
     (Machine : in Claw_Machine; Pushes : out Push_Count) return Boolean
   is
      --  Cramer's Rule.
      det  : constant Position :=
        Position (Machine.A.X) * Position (Machine.B.Y) -
        Position (Machine.B.X) * Position (Machine.A.Y);
      detA : constant Position :=
        Machine.Prize.X * Position (Machine.B.Y) -
        Position (Machine.B.X) * Machine.Prize.Y;
      detB : constant Position :=
        Position (Machine.A.X) * Machine.Prize.Y -
        Machine.Prize.X * Position (Machine.A.Y);
   begin
      if detA mod det = 0 and detB mod det = 0 then
         Pushes.A := Counter (detA / det);
         Pushes.B := Counter (detB / det);
         return True;
      else
         return False;
      end if;
   end Solution;
end Advent.Day_13;
