with Ada.Text_IO; use Ada.Text_IO;

package body Advent.Day_02 is
   procedure Get_Input_Entry (Levels : out Level_Vectors.Vector) is
      package Level_Text_IO is new Ada.Text_IO.Integer_IO (Level_Type);

      use Level_Text_IO;

      Line  : constant String := Get_Line;
      Start : Positive := Line'First;
      Level : Level_Type;
   begin
      Levels.Clear;
      Levels.Reserve_Capacity (32);
      while Start <= Line'Last loop
         Get (Line (Start .. Line'Last), Level, Start);
         Levels.Append (Level);
         Start := Start + 1;
      end loop;
   end Get_Input_Entry;

   function Is_Monotone (Levels   : Level_Vectors.Vector;
                         Min, Max : Integer) return Boolean is
   begin
      for I in Levels.First_Index + 1 .. Levels.Last_Index loop
         declare
            Distance : constant Integer := Levels.Element (I) - Levels.Element (I - 1);
         begin
            if Distance < Min or Distance > Max then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Is_Monotone;
end Advent.Day_02;
