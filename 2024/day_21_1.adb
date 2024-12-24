with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_21;       use Advent.Day_21;
with Advent;              use Advent;

procedure Day_21_1 is
   Total : Natural := 0;
begin
   loop
      declare
         Code : constant Numeric_Keys     := Get_Code (Standard_Input);
         Keys : constant Directional_Keys :=
           Translate (Translate (Translate (Code)));

         -- FIXME: Multiple translations of "379A", each of these
         -- translations may have multiple translations, these
         -- translations may have different lengths.
         Numeric_Code : constant Natural := To_Number (Code);
      begin
         if Debug then
            Put_Line
              (Standard_Error,
               Code'Image & Numeric_Code'Image & " =>" & Keys'Image &
               Keys'Length'Image);
         end if;

         Total := Total + Numeric_Code * Keys'Length;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_21_1;
