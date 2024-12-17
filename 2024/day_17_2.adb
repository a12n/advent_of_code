with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_17; use Advent.Day_17;
with Advent;        use Advent;

procedure Day_17_2 is
   CPU     : constant CPU_Type     := Get_CPU (Standard_Input);
   Program : constant Number_Array := Get_Program (Standard_Input);
begin
   Print (Standard_Error, Program);
   --  0: b = a & 0b111
   --  2: b = b ^ 4
   --  4: c = a >> b
   --  6: b = b ^ c
   --  8: b = b ^ 4
   --  10: output = b & 0b111
   --  12: a = a >> 3
   --  14: if a != 0 { goto 0 }

   --  2: b = (a & 0b111) ^ 0b100
   --  8: b = (b ^ (a >> b)) ^ 0b100
   --  10: output = b & 0b111
   --  12: a = a >> 3
   --  14: if a != 0 { goto 0 }
end Day_17_2;
