with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Advent.Day_05; use Advent.Day_05;

procedure Day_05_1 is
   Order : constant Precedence := Input_Precedence (Standard_Input);
   Sum   : Natural := 0;
begin
   Put (Standard_Error, Order'Image);
   loop
      declare
         Pages : constant Page_Array := Input_Pages (Standard_Input);
      begin
         Put_Line (Standard_Error, Pages'Image);
         if In_Order (Order, Pages) then
            Put_Line (Standard_Error, True'Image);
            Sum := Sum + Pages (Pages'First + (Pages'Last - Pages'First) / 2);
         end if;
      end;
   end loop;
exception when End_Error =>
   Put (Sum, 0);
   New_Line;
end Day_05_1;
