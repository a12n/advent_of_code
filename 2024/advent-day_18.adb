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

   procedure Print (File : File_Type; Corrupted : Corrupted_Map) is
   begin
      for Row in Corrupted'Range (1) loop
         for Col in Corrupted'Range (2) loop
            if Corrupted (Row, Col) then
               Put (File, '#');
            else
               Put (File, '.');
            end if;
         end loop;
         New_Line (File);
      end loop;
   end Print;

   function Shortest_Path
     (Corrupted : Corrupted_Map; Start_Pos, Finish_Pos : Position)
      return Natural
   is
   begin
      --  TODO
      return Natural'Last;
   end Shortest_Path;
end Advent.Day_18;
