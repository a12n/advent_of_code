package body Advent.Day_15 is
   function Get_Move (File : File_Type) return Direction is
      Char : Character;
   begin
      Get (File, Char);
      case Char is
         when '<' =>
            return Left;
         when 'v' =>
            return Down;
         when '>' =>
            return Right;
         when '^' =>
            return Up;
         when others =>
            raise Constraint_Error;
      end case;
   end Get_Move;

   function Get_Warehouse
     (File : File_Type; Robot_Pos : out Position) return Warehouse_Map
   is
      Line      : constant String                        := Get_Line (File);
      Warehouse : Warehouse_Map (Line'Range, Line'Range) :=
        [others => [others => Wall]];

      procedure Process (Pos : Position; Char : Character) is
      begin
         case Char is
            when '.' =>
               Warehouse (Pos (1), Pos (2)) := Empty;
            when '#' =>
               Warehouse (Pos (1), Pos (2)) := Wall;
            when 'O' =>
               Warehouse (Pos (1), Pos (2)) := Box;
            when '@' =>
               Warehouse (Pos (1), Pos (2)) := Robot;
               Robot_Pos                    := Pos;
            when others =>
               raise Constraint_Error;
         end case;
      end Process;
   begin
      for Col in Line'Range loop
         Process ([1, Col], Line (Col));
      end loop;
      for Row in Warehouse'First (1) + 1 .. Warehouse'Last (1) loop
         for Col in Warehouse'Range (2) loop
            declare
               Next : Character;
            begin
               Get (File, Next);
               Process ([Row, Col], Next);
            end;
         end loop;
      end loop;
      return Warehouse;
   end Get_Warehouse;

   function Move
     (Warehouse : in out Warehouse_Map; Pos : Position; Dir : Direction)
      return Position
   is
      Next      : Position := Pos + To_Offset (Dir);
      This_Tile : Tile renames Warehouse (Pos (1), Pos (2));
      Next_Tile : Tile renames Warehouse (Next (1), Next (2));
   begin
      --  Put_Line
      --    (Standard_Error,
      --     "Pos " & Pos'Image & ", Dir " & Dir'Image & ", Next " & Next'Image);
      --  Put_Line (Standard_Error, "This " & This_Tile'Image);
      case Next_Tile is
         when Box =>
            --  Put_Line (Standard_Error, "Next " & Next_Tile'Image);
            if Move (Warehouse, Next, Dir) /= Next then
               Next_Tile := This_Tile;
               This_Tile := Empty;
               return Next;
            else
               return Pos;
            end if;
         when Empty =>
            --  Put_Line (Standard_Error, "Next " & Next_Tile'Image);
            Next_Tile := This_Tile;
            This_Tile := Empty;
            return Next;
         when Wall =>
            --  Put_Line (Standard_Error, "Next " & Next_Tile'Image);
            return Pos;
         when others =>
            --  Put_Line (Standard_Error, "Next " & Next_Tile'Image);
            raise Constraint_Error;
      end case;
   end Move;

   procedure Print (File : File_Type; Warehouse : Warehouse_Map) is
   begin
      for Row in Warehouse'Range (1) loop
         for Col in Warehouse'Range (2) loop
            Put (File, To_Character (Warehouse (Row, Col)));
         end loop;
         New_Line (File);
      end loop;
   end Print;

   procedure Print (File : File_Type; Warehouse : Wide_Warehouse_Map) is
   begin
      for Row in Warehouse'Range (1) loop
         for Col in Warehouse'Range (2) loop
            Put (File, To_Character (Warehouse (Row, Col)));
         end loop;
         New_Line (File);
      end loop;
   end Print;

   function Widen
     (Warehouse : Warehouse_Map; Robot_Pos : out Position)
      return Wide_Warehouse_Map
   is
      Wide_Warehouse :
        Wide_Warehouse_Map
          (Warehouse'Range (1), Warehouse'First (2) .. 2 * Warehouse'Last (2));
   begin
      for Row in Warehouse'Range (1) loop
         for Col in Warehouse'Range (2) loop
            declare
               This_Tile   : Tile renames Warehouse (Row, Col);
               Wide_Tile_1 :
                 Wide_Tile renames Wide_Warehouse (Row, 2 * Col - 1);
               Wide_Tile_2 : Wide_Tile renames Wide_Warehouse (Row, 2 * Col);
            begin
               case This_Tile is
                  when Empty | Wall =>
                     Wide_Tile_1 := This_Tile;
                     Wide_Tile_2 := This_Tile;
                  when Box =>
                     Wide_Tile_1 := Box_Left;
                     Wide_Tile_2 := Box_Right;
                  when Robot =>
                     Robot_Pos   := [Row, 2 * Col - 1];
                     Wide_Tile_1 := Robot;
                     Wide_Tile_2 := Empty;
               end case;
            end;
         end loop;
      end loop;
      return Wide_Warehouse;
   end Widen;
end Advent.Day_15;
