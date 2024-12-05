with Ada.Containers.Generic_Array_Sort;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_05;       use Advent.Day_05;

procedure Day_05_2 is
   Order : constant Precedence := Input_Precedence (Standard_Input);
   Sum   : Natural             := 0;

   function Before (A, B : Page_Number) return Boolean is
   begin
      return Order (A, B);
   end Before;

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Positive, Page_Number, Page_Array, Before);
begin
   loop
      declare
         Pages : Page_Array := Input_Pages (Standard_Input);
      begin
         if not In_Order (Order, Pages) then
            Sort (Pages);
            Sum := Sum + Pages (Pages'First + (Pages'Last - Pages'First) / 2);
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Sum, 0);
      New_Line;
end Day_05_2;
