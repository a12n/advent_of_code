with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_14;       use Advent.Day_14;
with Advent;              use Advent;

procedure Day_14_1 is
   Robots    : Robot_Array               := Input (Standard_Input);
   Quadrants : array (1 .. 4) of Natural := [others => 0];
begin
   if Debug then
      Put_Line (Standard_Error, Robots'Image);
   end if;
   Simulate (Robots, 100);

   for Robot of Robots loop
      if Robot.P.X < X_Position'Last / 2 then
         if Robot.P.Y < Y_Position'Last / 2 then
            Quadrants (1) := @ + 1;
         elsif Robot.P.Y > Y_Position'Last / 2 then
            Quadrants (3) := @ + 1;
         end if;
      elsif Robot.P.X > X_Position'Last / 2 then
         if Robot.P.Y < Y_Position'Last / 2 then
            Quadrants (2) := @ + 1;
         elsif Robot.P.Y > Y_Position'Last / 2 then
            Quadrants (4) := @ + 1;
         end if;
      end if;
   end loop;

   if Debug then
      Put_Line (Standard_Error, Quadrants'Image);
   end if;
   Put (Quadrants (1) * Quadrants (2) * Quadrants (3) * Quadrants (4), 0);
   New_Line;
end Day_14_1;
