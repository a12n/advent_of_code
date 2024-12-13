with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Advent.Day_13 is
   function Input_Entry (File : File_Type) return Claw_Machine is
      A_Line     : String   := Get_Line (File);
      B_Line     : String   := Get_Line (File);
      Prize_Line : String   := Get_Line (File);
      Start      : Positive := 1;
      Machine    : Claw_Machine;

      procedure Replace_Non_Digits (Line : in out String) is
      begin
         for I in Line'Range loop
            if Line (I) not in '0' .. '9' then
               Line (I) := ' ';
            end if;
         end loop;
      end Replace_Non_Digits;
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

      Replace_Non_Digits (A_Line);
      Replace_Non_Digits (B_Line);
      Replace_Non_Digits (Prize_Line);

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
      Min_Cost : Natural := Natural'Last;
   begin
      for N in 0 .. 100 loop
         for M in 0 .. 100 loop
            if Machine.A.X * N + Machine.B.X * M = Machine.Prize.X and
              Machine.A.Y * N + Machine.B.Y * M = Machine.Prize.Y
            then
               if Cost ((N, M)) < Min_Cost then
                  Min_Cost := Cost ((N, M));
                  Pushes   := (N, M);
               end if;
            end if;
         end loop;
      end loop;
      return Min_Cost /= Natural'Last;
   end Solution;
end Advent.Day_13;
