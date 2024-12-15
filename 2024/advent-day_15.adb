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
     (Warehouse : in out Warehouse_Map; Pos : in out Position; Dir : Direction)
      return Boolean
   is
      Next      : Position := Pos + To_Offset (Dir);
      This_Tile : Tile renames Warehouse (Pos (1), Pos (2));
      Next_Tile : Tile renames Warehouse (Next (1), Next (2));
   begin
      case Next_Tile is
         when Box =>
            if Move (Warehouse, Next, Dir) then
               Next_Tile := This_Tile;
               This_Tile := Empty;
               Pos       := Next;
               return True;
            else
               return False;
            end if;
         when Empty =>
            Next_Tile := This_Tile;
            This_Tile := Empty;
            Pos       := Next;
            return True;
         when Wall =>
            return False;
         when others =>
            raise Constraint_Error;
      end case;
   end Move;
end Advent.Day_15;
