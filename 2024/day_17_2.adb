with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_17; use Advent.Day_17;
with Advent;        use Advent;
with Interfaces;    use Interfaces;

procedure Day_17_2 is
   Initial : constant CPU_Type     := Get_CPU (Standard_Input);
   Program : constant Number_Array := Get_Program (Standard_Input);
   Answer  : Unsigned_64           := 0;
begin
   Print (Standard_Error, Program);
   for I in Program'Range loop
      for V in 0 .. 2**7 loop
         declare
            CPU    : CPU_Type := Initial;
            Output : Number;
         begin
            CPU.R (A) := Register (V);
            if CPU.Run (Program, Output) and then Output = Program (I) then
               Put_Line
                 (Standard_Error,
                  "I " & I'Image & ", Program(I) " & Program (I)'Image &
                  ", V " & Unsigned_64'Image (Unsigned_64 (V) and 2#111#));
               Answer :=
                 Shift_Left (Answer, 3) or (Unsigned_64 (V) and 2#111#);
               exit;
            end if;
         end;
      end loop;
   end loop;
   Put_Line (Standard_Error, Answer'Image);
end Day_17_2;
