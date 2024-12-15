package body Advent.Day_06 is
   function Input
     (File : File_Type; Guard_Pos : out Position; Guard_Dir : out Direction)
      return Blocked_Map
   is
      Line : constant String := Get_Line (File);

      Blocked : Blocked_Map (Line'Range, Line'Range) :=
        [others => [others => False]];

      procedure Process (Pos : Position; Char : Character) is
      begin
         case Char is
            when '.' | 'o' =>
               null;
            when '#' =>
               Blocked (Pos (1), Pos (2)) := True;
            when '^' =>
               Guard_Pos := Pos;
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

   procedure Walk
     (Blocked :    Blocked_Map; Guard_Pos : Position; Guard_Dir : Direction;
      Visited : in out Visited_Map)
   is
      Dir  : Direction := Guard_Dir;
      Pos  : Position  := Guard_Pos;
      Next : Position;
   begin
      loop
         if not Visited (Pos (1), Pos (2), Dir) then
            Visited (Pos (1), Pos (2), Dir) := True;
         else
            raise Loop_Error;
         end if;
         Next := Pos + To_Offset (Dir);
         exit when Next (1) not in Visited'Range (1) or
           Next (2) not in Visited'Range (2);
         if Blocked (Next (1), Next (2)) then
            Dir := Rotate (CW, Dir);
         else
            Pos := Next;
         end if;
      end loop;
   end Walk;

   procedure Walk
     (Blocked :    Blocked_Map; Guard_Pos : Position; Guard_Dir : Direction;
      Visited : in out Visited_Map; Number_Loops : out Natural)
   is
      Dir  : Direction := Guard_Dir;
      Pos  : Position  := Guard_Pos;
      Next : Position;

      Added : Blocked_Map (Blocked'Range (1), Blocked'Range (2)) :=
        [others => [others => False]];
   begin
      loop
         if not Visited (Pos (1), Pos (2), Dir) then
            Visited (Pos (1), Pos (2), Dir) := True;
         else
            raise Loop_Error;
         end if;
         Next := Pos + To_Offset (Dir);
         exit when Next (1) not in Visited'Range (1) or
           Next (2) not in Visited'Range (2);
         if Blocked (Next (1), Next (2)) then
            Dir := Rotate (CW, Dir);
         else
            declare
               Blocked2 : Blocked_Map := Blocked;
               Visited2 :
                 Visited_Map
                   (Blocked2'Range (1), Blocked2'Range (2), Direction'Range) :=
                 [others => [others => [others => False]]];
            begin
               Blocked2 (Next (1), Next (2)) := True;
               Walk (Blocked2, Guard_Pos, Guard_Dir, Visited2);
            exception
               when Loop_Error =>
                  Added (Next (1), Next (2)) := True;
            end;
            Pos := Next;
         end if;
      end loop;

      Number_Loops := 0;
      for Row in Added'Range (1) loop
         for Col in Added'Range (2) loop
            if Added (Row, Col) then
               Number_Loops := Number_Loops + 1;
            end if;
         end loop;
      end loop;
   end Walk;
end Advent.Day_06;
