with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_24; use Advent.Day_24;
with Advent;        use Advent;

procedure Day_24_2 is
   use Number_Text_IO;
   --  45 x input wires
   --  45 y input wires
   --  46 z output wires
   --  176 gates
   Wires    : constant Wire_Map    := Get_Wires (Standard_Input);
   Expected : constant Number_Type :=
     Number (Wires, 'x') + Number (Wires, 'y');
begin
   Put (Standard_Error, "x ");
   Put (Standard_Error, Number (Wires, 'x'), 0);
   New_Line (Standard_Error);

   Put (Standard_Error, "y ");
   Put (Standard_Error, Number (Wires, 'y'), 0);
   New_Line (Standard_Error);

   Put (Standard_Error, "expect ");
   Put (Standard_Error, Expected, 0);
   New_Line (Standard_Error);

   Put (Standard_Error, "actual ");
   Put (Standard_Error, Number (Wires, 'z'), 0);
   New_Line (Standard_Error);
end Day_24_2;
