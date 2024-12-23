with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_23;       use Advent.Day_23;
with Advent;              use Advent;

procedure Day_23_1 is
   Online    : Address_Set;
   Connected : constant Connection_Map :=
     Get_Connections (Standard_Input, Online);
   N         : Natural                 := 0;
begin
   for A in Address'Range loop
      if Online (A) then
         for B in A + 1 .. Address'Last loop
            if Connected (A) (B) then
               for C in B + 1 .. Address'Last loop
                  if Connected (B) (C) and Connected (C) (A) and
                    (A in Historian_Address'Range or
                     B in Historian_Address'Range or
                     C in Historian_Address'Range)
                  then
                     N := N + 1;
                     if Debug then
                        Put_Line
                          (Standard_Error,
                           To_String (A) & ',' & To_String (B) & ',' &
                           To_String (C));
                     end if;
                  end if;
               end loop;
            end if;
         end loop;
      end if;
   end loop;

   Put (N, 0);
   New_Line;
end Day_23_1;
