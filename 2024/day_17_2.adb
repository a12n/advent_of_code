with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_17; use Advent.Day_17;
with Advent;        use Advent;
with Interfaces;    use Interfaces;

procedure Day_17_2 is
   use Register_Text_IO;
   Initial : constant CPU_Type     := Get_CPU (Standard_Input);
   Program : constant Number_Array := Get_Program (Standard_Input);
   Answer  : Unsigned_64           := 0;

   function Compiled
     (CPU : in out CPU_Type; Output : out Number) return Boolean
   is
   begin
      CPU.R (B) := CPU.R (A) and 2#111#;
      CPU.R (B) := CPU.R (B) xor 4;
      CPU.R (C) :=
        Register (Shift_Right (Unsigned_64 (CPU.R (A)), Natural (CPU.R (B))));
      CPU.R (B) := CPU.R (B) xor CPU.R (C);
      CPU.R (B) := CPU.R (B) xor 4;
      Output    := Number (CPU.R (B) and 2#111#);
      CPU.R (A) := Register (Shift_Right (Unsigned_64 (CPU.R (A)), 3));
      return CPU.R (A) /= 0;
   end Compiled;
begin
   Print (Standard_Error, Program);
   for Value in
      --  0
      --  2             2#______________________________10#
      --  251           2#________________________11111011#
      --  1_538         2#_____________________11000000010#
      --  214_267       2#______________110100010011111011#
      --  218_363       2#______________110101010011111011#
      --  460_290       2#_____________1110000011000000010#
      --  42_403_330    2#______10100001110000011000000010#
      --  115_803_650   2#_____110111001110000011000000010#
      --  518_456_834   2#___11110111001110000011000000010#
      --  4_276_553_218 2#11111110111001110000011000000010#
      4_276_553_218 .. Register'Last loop
      declare
         CPU    : CPU_Type := Initial;
         Output : Number;
      begin
         CPU.R (A) := Register (Value);
         if CPU.Run (Program, Output) and then Output = Program (1)
           and then CPU.Run (Program, Output) and then Output = Program (2)
           and then CPU.Run (Program, Output) and then Output = Program (3)
           and then CPU.Run (Program, Output) and then Output = Program (4)
           and then CPU.Run (Program, Output) and then Output = Program (5)
           and then CPU.Run (Program, Output) and then Output = Program (6)
           and then CPU.Run (Program, Output) and then Output = Program (7)
           and then CPU.Run (Program, Output) and then Output = Program (8)
           and then CPU.Run (Program, Output) and then Output = Program (9)
           and then CPU.Run (Program, Output) and then Output = Program (10)
            --  and then CPU.Run (Program, Output) and then Output = Program (11)
            --  and then CPU.Run (Program, Output) and then Output = Program (12)
            --  and then CPU.Run (Program, Output) and then Output = Program (13)
            --  and then CPU.Run (Program, Output) and then Output = Program (14)
            --  and then CPU.Run (Program, Output) and then Output = Program (15)
            --  and then CPU.Run (Program, Output) and then Output = Program (16)

         then
            Put (Standard_Error, Register (Value), Base => 10);
            Put (Standard_Error, ' ');
            Put (Standard_Error, Register (Value), Base => 2, Width => 64);
            New_Line (Standard_Error);

            --  & Unsigned_64 (Value)'Image &
            --                   Unsigned_64'Image (Unsigned_64 (Value) and 2#111#));
            --  Answer :=
            --    Shift_Left (Answer, 3) or (Unsigned_64 (V) and 2#111#);
            --  exit;
         end if;
      end;
   end loop;
   Put_Line (Standard_Error, Answer'Image);
end Day_17_2;
