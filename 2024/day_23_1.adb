with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_23; use Advent.Day_23;

procedure Day_23_1 is
   Connections : constant Connection_Map := Get_Connections (Standard_Input);
   Components      : Component_Map;
   Component_Sizes : constant Component_Size_Array :=
     Connected_Components (Connections, Components);
begin
   Put_Line (Standard_Error, "Connections" & Connections'Image);
   Put_Line (Standard_Error, "Components" & Components'Image);
   Put_Line (Standard_Error, "Component_Sizes" & Component_Sizes'Image);
   --  TODO
end Day_23_1;
