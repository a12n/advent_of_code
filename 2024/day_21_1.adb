with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_21;       use Advent.Day_21;
with Advent;              use Advent;

procedure Day_21_1 is
   Total : Natural := 0;
begin
   loop
      declare
         Code : constant Numeric_Presses := Get_Code (Standard_Input);

         --  FIXME: Keep state (arm positions) between codes?
         Presses : constant Directional_Presses :=
           Translate (Translate (Translate (Code)));

         Numeric_Code : constant Natural := To_Number (Code);
      begin
         if Debug then
            Put_Line
              (Standard_Error,
               Code'Image & Numeric_Code'Image & " =>" & Presses'Image &
               Presses'Length'Image);
         end if;

         Total := Total + Numeric_Code * Presses'Length;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_21_1;
