with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers;      use Ada.Containers;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Advent.Day_18 is
   function Get_Byte_Position (File : File_Type) return Position is
      Line  : String := Get_Line (File);
      Pos   : Position;
      Start : Positive;
   begin
      if Line'Length = 3 and Line (2) = ',' then
         Line (2) := ' ';
      elsif Line'Length = 4 and Line (2) = ',' then
         Line (2) := ' ';
      elsif Line'Length = 4 and Line (3) = ',' then
         Line (3) := ' ';
      elsif Line'Length = 5 and Line (3) = ',' then
         Line (3) := ' ';
      else
         raise Constraint_Error;
      end if;
      Get (Line, Pos (2), Start);
      Get (Line (Start + 1 .. Line'Last), Pos (1), Start);
      return Pos;
   end Get_Byte_Position;

   procedure Print
     (File : File_Type; Positions : Position_Map; No : Character := '.';
      Yes  : Character := '#')
   is
   begin
      for Row in Positions'Range (1) loop
         for Col in Positions'Range (2) loop
            if Positions (Row, Col) then
               Put (File, Yes);
            else
               Put (File, No);
            end if;
         end loop;
         New_Line (File);
      end loop;
   end Print;

   function Shortest_Path
     (Corrupted :     Position_Map; Start_Pos, Finish_Pos : Position;
      Distance  : out Natural) return Boolean
   is
      Unused_Path : Position_Map (Corrupted'Range (1), Corrupted'Range (2));
   begin
      return
        Shortest_Path
          (Corrupted, Start_Pos, Finish_Pos, Unused_Path, Distance);
   end Shortest_Path;

   function Shortest_Path
     (Corrupted :     Position_Map; Start_Pos, Finish_Pos : Position;
      Path      : out Position_Map; Distance : out Natural) return Boolean
   is
      type State is record
         Pos  : Position;
         Dist : Natural;
      end record;

      function Get_Priority (S : State) return Natural is (S.Dist);
      function Before (N, M : Natural) return Boolean renames "<";

      package State_Queue_Interface is new Synchronized_Queue_Interfaces
        (State);
      package State_Queues is new Unbounded_Priority_Queues
        (Queue_Interfaces => State_Queue_Interface, Queue_Priority => Natural);

      use State_Queues;

      Q : Queue;
      S : State;

      type Parent_Link is record
         Valid : Boolean;
         Dir   : Direction;
      end record;

      Parent :
        array (Corrupted'Range (1), Corrupted'Range (2)) of Parent_Link :=
        [others => [others => (Valid => False, Dir => Down)]];
   begin
      Q.Enqueue ((Start_Pos, 0));
      Parent (Start_Pos (1), Start_Pos (2)).Valid :=
        True; --  Arbitrary direction.

      while Q.Current_Use > 0 loop
         Q.Dequeue (S);

         exit when S.Pos = Finish_Pos;

         for Dir in Direction'Range loop
            declare
               Next : constant Position := S.Pos + To_Offset (Dir);
            begin
               if Next (1) in Corrupted'Range (1)
                 and then Next (2) in Corrupted'Range (2)
                 and then not Corrupted (Next (1), Next (2))
                 and then not Parent (Next (1), Next (2)).Valid
               then
                  Q.Enqueue ((Next, S.Dist + 1));
                  Parent (Next (1), Next (2)) :=
                    (Valid => True, Dir => Opposite (Dir));
               end if;
            end;
         end loop;
      end loop;

      if S.Pos /= Finish_Pos then
         return False;
      end if;

      Distance := S.Dist;
      while S.Pos /= Start_Pos loop
         Path (S.Pos (1), S.Pos (2)) := True;
         pragma Assert (Parent (S.Pos (1), S.Pos (2)).Valid);
         S.Pos := S.Pos + To_Offset (Parent (S.Pos (1), S.Pos (2)).Dir);
      end loop;
      Path (Start_Pos (1), Start_Pos (2)) := True;

      return True;
   end Shortest_Path;
end Advent.Day_18;
