with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_17; use Advent.Day_17;
with Advent;        use Advent;

procedure Day_17_2 is
   use Register_Text_IO;

   CPU     : constant CPU_Type     := Get_CPU (Standard_Input);
   Program : constant Number_Array := Get_Program (Standard_Input);

   function Quine (Initial : Register) return Boolean is
      Temp_CPU : CPU_Type := CPU;
      Output   : Number;
   begin
      Temp_CPU.R (A) := Initial;
      --  If program outputs itself instructions…
      for I in Program'Range loop
         if Temp_CPU.Run (Program, Output) then
            if Output /= Program (I) then
               return False;
            end if;
         else
            return False;
         end if;
      end loop;
      --  …and then halts.
      return not Temp_CPU.Run (Program, Output);
   end Quine;
begin
   Print (Standard_Error, Program);
   return;
   for Initial in Register'Range loop
      if Debug then
         Put_Line (Standard_Error, Initial'Image);
      end if;
      if Quine (Initial) then
         Put (Initial, 0);
         New_Line;
         exit;
      end if;
   end loop;
end Day_17_2;
