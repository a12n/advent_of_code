with Ada.Text_IO;   use Ada.Text_IO;
with Advent.ANSI;   use Advent.ANSI;
with Advent.Day_15; use Advent.Day_15;
with Advent.Grids;  use Advent.Grids;

procedure Day_15_2 is
   Robot_Pos : Position;
   Warehouse : Wide_Warehouse_Map :=
     Widen (Get_Warehouse (Standard_Input, Robot_Pos), Robot_Pos);

   Next_Robot_Pos : Position;
   Next_Warehouse :
     Wide_Warehouse_Map (Warehouse'Range (1), Warehouse'Range (2));
begin
   --  Put_Line (Standard_Error, "Robot_Pos" & Robot_Pos'Image);

   Put (Standard_Error, Hide_Cursor);
   Put_Line (Standard_Error, Cursor_Top_Left);
   Print (Standard_Error, Warehouse);   --  50x100
   begin
      for I in Positive'Range loop
         delay 0.33;

         Next_Warehouse := Warehouse;
         Next_Robot_Pos :=
           Move (Next_Warehouse, Robot_Pos, Get_Move (Standard_Input));
         if Next_Robot_Pos /= Robot_Pos then
            Robot_Pos := Next_Robot_Pos;
            Warehouse := Next_Warehouse;
         end if;

         Put_Line (Standard_Error, Cursor_Top_Left & I'Image);
         Print (Standard_Error, Warehouse);
      end loop;
   exception
      when End_Error =>
         null;
   end;
   Put (Standard_Error, Show_Cursor);

   --  TODO
end Day_15_2;
