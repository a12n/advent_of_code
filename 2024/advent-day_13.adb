with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Advent.Day_13 is
   function Input_Entry (File : File_Type) return Claw_Machine is
      A_Line     : constant String := Get_Line (File);
      B_Line     : constant String := Get_Line (File);
      Prize_Line : constant String := Get_Line (File);
      Start      : Positive        := 1;
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

      Replace_Non_Digits (A_Line);
      Replace_Non_Digits (B_Line);
      Replace_Non_Digits (Prize_Line);

      Get (A_Line, Machine.A (1), Start);
      Get (A_Line (Start + 1 .. A_Line'Last), Machine.A (2), Start);
      Get (B_Line, Machine.B (1), Start);
      Get (B_Line (Start + 1 .. B_Line'Last), Machine.B (2), Start);
      Get (Prize_Line, Machine.Prize (1), Start);
      Get
        (Prize_Line (Start + 1 .. Prize_Line'Last), Machine.Prize (2), Start);

      return Machine;
   end Input_Entry;
end Advent.Day_13;
