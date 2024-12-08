package body Advent.Day_06 is
   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Boolean_Grid
   is
      Line : constant String := Get_Line (File);

      Blocked : Boolean_Grid (Line'Range, Line'Range) :=
        [others => [others => False]];

      procedure Process (Pos : Position; Char : Character) is
      begin
         case Char is
            when '.' =>
               null;
            when '#' =>
               Blocked (Pos (1), Pos (2)) := True;
            when '^' =>
               Guard     := Pos;
               Guard_Dir := Up;
            when others =>
               raise Constraint_Error;
         end case;
      end Process;
   begin
      for Col in Line'Range loop
         Process ([1, Col], Line (Col));
      end loop;
      for Row in 2 .. Blocked'Last (1) loop
         for Col in Blocked'Range (2) loop
            declare
               Next : Character;
            begin
               Get (File, Next);
               Process ([Row, Col], Next);
            end;
         end loop;
      end loop;
      return Blocked;
   end Input;

   function Walk
     (Blocked : Boolean_Grid; Guard : Position; Guard_Dir : Direction)
      return Boolean_Grid
   is
      Visited : Boolean_Grid (Blocked'Range (1), Blocked'Range (2)) :=
        [others => [others => False]];
      Current : Position := Guard;
      Next    : Position;
      Dir     : Direction := Guard_Dir;
   begin
      loop
         Visited (Current (1), Current (2)) := True;
         Next                               := Current + To_Offset (Dir);
         exit when Next (1) not in Visited'Range (1) or
           Next (2) not in Visited'Range (2);
         if Blocked (Next (1), Next (2)) then
            Dir := Rotate (CW, Dir);
         else
            Current := Next;
         end if;
      end loop;
      return Visited;
   end Walk;
end Advent.Day_06;
