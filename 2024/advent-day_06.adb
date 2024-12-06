package body Advent.Day_06 is
   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Obstruction_Map
   is
      Line         : constant String := Get_Line (File);
      Obstructions : Obstruction_Map (Line'Range, Line'Range);

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
      Visited : Visited_Map (Obstructions'Range (1), Obstructions'Range (2));
   begin
      return Visited;
   end Walk;
end Advent.Day_06;
