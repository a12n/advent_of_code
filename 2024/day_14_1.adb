with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_14; use Advent.Day_14;

procedure Day_14_1 is
   Robots    : Robot_Array               := Input (Standard_Input);
   Quadrants : array (1 .. 4) of Natural := [others => 0];
begin
   Put_Line (Standard_Error, Robots'Image);
   Simulate (Robots, 100);

   for I in Robots'Range loop
      if Robots (I).P.X < X_Position'Last / 2 then
         if Robots (I).P.Y < Y_Position'Last / 2 then
            Quadrants (1) := @ + 1;
         elsif Robots (I).P.Y > Y_Position'Last / 2 then
            Quadrants (3) := @ + 1;
         end if;
      elsif Robots (I).P.X > X_Position'Last / 2 then
         if Robots (I).P.Y < Y_Position'Last / 2 then
            Quadrants (2) := @ + 1;
         elsif Robots (I).P.Y > Y_Position'Last / 2 then
            Quadrants (4) := @ + 1;
         end if;
      end if;
   end loop;

   Put_Line (Standard_Error, Quadrants'Image);
end Day_14_1;
