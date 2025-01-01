with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_21; use Advent.Day_21;

procedure Day_21_2 is
   use Count_Text_IO;

   Total      : Count_Type := 0;
   Translator : Translator_Type (25);
begin
   loop
      declare
         Code   : constant Numeric.Code_Type     :=
           Numeric.Get_Code (Standard_Input);
         N      : constant Natural               := Numeric.To_Number (Code);
         Keys   : constant Directional.Key_Array := Translate (Code);
         Length : constant Count_Type            :=
           Translate_Length (Translator, Keys, 25);
      begin
         Total := Total + Count_Type (N) * Length;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_21_2;
