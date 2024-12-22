with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_21;       use Advent.Day_21;
with Advent;              use Advent;

procedure Day_21_1 is
   Total : Natural := 0;
begin
   loop
      declare
         Code    : constant Numeric_Presses     := Get_Code (Standard_Input);
         --  FIXME: Keep state (arm positions) between codes?
         Presses : constant Directional_Presses :=
           Translate (Translate (Translate (Translate (Code))));
      begin
         if Debug then
            Put_Line (Standard_Error, Code'Image);
            Put_Line (Standard_Error, Presses'Image);
         end if;

         Total := Total + To_Number (Code) * Presses'Length;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_21_1;
