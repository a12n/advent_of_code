package body Advent.Day_06 is
   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Obstruction_Map
   is
      Line         : constant String := Get_Line (File);
      Obstructions : Obstruction_Map (Line'Range, Line'Range) :=
        [others => [others => False]];

      procedure Process (Row, Col : Positive; Char : Character) is
      begin
         case Char is
            when '.' =>
               null;
            when '#' =>
               Obstructions (Row, Col) := True;
            when '^' =>
               Guard     := [Row, Col];
               Guard_Dir := Up;
            when others =>
               raise Constraint_Error;
         end case;
      end Process;
   begin
      for Col in Line'Range loop
         Process (1, Col, Line (Col));
      end loop;
      for Row in 2 .. Obstructions'Last (1) loop
         for Col in Obstructions'Range (2) loop
            declare
               Next : Character;
            begin
               Get (File, Next);
               Process (Row, Col, Next);
            end;
         end loop;
      end loop;
      return Obstructions;
   end Input;

   function Walk
     (Obstructions : Obstruction_Map; Guard : Position; Guard_Dir : Direction)
      return Visited_Map
   is
      function Advance (Pos : Position; Dir : Direction) return Position is
      begin
         case Dir is
            when Down =>
               return [Pos (1) + 1, Pos (2)];
            when Left =>
               return [Pos (1), Pos (2) - 1];
            when Right =>
               return [Pos (1), Pos (2) + 1];
            when Up =>
               return [Pos (1) - 1, Pos (2)];
         end case;
      end Advance;

      function Rotate (Dir : Direction) return Direction is
      begin
         case Dir is
            when Down =>
               return Left;
            when Left =>
               return Up;
            when Right =>
               return Down;
            when Up =>
               return Right;
         end case;
      end Rotate;

      Visited : Visited_Map (Obstructions'Range (1), Obstructions'Range (2)) :=
        [others => [others => False]];
      Pos     : Position := Guard;
      Dir     : Direction := Guard_Dir;
   begin
      Put_Line (Standard_Error, "Obstructions" & Obstructions'Image);
      loop
         declare
            Next : constant Position := Advance (Pos, Dir);
         begin
            Put_Line
              (Standard_Error, "Pos " & Pos'Image & ", Dir " & Dir'Image);
            Visited (Pos (1), Pos (2)) := True;
            if Obstructions (Next (1), Next (2)) then
               Dir := Rotate (Dir);
            else
               Pos := Next;
            end if;
         exception
            when Constraint_Error =>
               return Visited;
         end;
      end loop;
   end Walk;
end Advent.Day_06;
