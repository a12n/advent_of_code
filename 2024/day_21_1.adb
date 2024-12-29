with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_21;       use Advent.Day_21;
with Advent.Debug;        use Advent.Debug;

procedure Day_21_1 is
   Total : Natural := 0;
begin
   loop
      declare
         Code : constant Numeric.Code_Type     :=
           Numeric.Get_Code (Standard_Input);
         N    : constant Natural               := Numeric.To_Number (Code);
         Keys : constant Directional.Key_Array :=
         --  You (directional keypad)
         Translate (
         --  Robot 1 (directional keypad)
         Translate (
         --  Robot 2 (directional keypad)
         Translate (
         --  Robot (numeric keypad)
         Code)));
      begin
         if Debug_Enabled then
            Put_Line (Standard_Error, "N " & N'Image);
         end if;
         Total := Total + N * Keys'Length;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_21_1;
