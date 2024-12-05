with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Advent.Day_05 is
   function Input_Precedence (File : File_Type) return Precedence is
      Result : Precedence := [others => [others => False]];
   begin
      loop
         declare
            Line : constant String := Get_Line (File);
            Before, After : Page_Number;
            Stop : Positive;
         begin
            exit when Line'Length = 0;
            Get (Line, Before, Stop);
            Get (Line (Stop + 2 .. Line'Last), After, Stop);
            Result (Before, After) := True;
         end;
      end loop;
      return Result;
   end Input_Precedence;
end Advent.Day_05;
