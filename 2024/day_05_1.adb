with Ada.Text_IO; use Ada.Text_IO;
with Advent.Day_05; use Advent.Day_05;

procedure Day_05_1 is
   Before : constant Precedence := Input_Precedence (Standard_Input);
begin
   Put (Standard_Error, Before'Image);
   loop
      declare
         Pages : constant Page_Array := Input_Pages (Standard_Input);
      begin
         Put_Line (Standard_Error, "Pages" & Pages'Image);
      end;
   end loop;
exception when End_Error =>
   null;
end Day_05_1;
