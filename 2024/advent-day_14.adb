with Ada.Integer_Text_IO;

package body Advent.Day_14 is
   function Input (File : File_Type) return Robot_Array is
      package X_Position_Text_IO is new Ada.Text_IO.Modular_IO (X_Position);
      package Y_Position_Text_IO is new Ada.Text_IO.Modular_IO (Y_Position);

      use Ada.Integer_Text_IO;
      use X_Position_Text_IO;
      use Y_Position_Text_IO;

      function Input_Line (Result : Robot_Array) return Robot_Array is
      begin
         declare
            Line  : String   := Get_Line (File);
            Start : Positive := 1;
            Robot : Robot_Type;
         begin
            for I in Line'Range loop
               case Line (I) is
                  when '-' | '0' .. '9' =>
                     null;
                  when others =>
                     Line (I) := ' ';
               end case;
            end loop;

            Get (Line (Start .. Line'Last), Robot.P.X, Start);
            Get (Line (Start + 1 .. Line'Last), Robot.P.Y, Start);
            Get (Line (Start + 1 .. Line'Last), Robot.V.X, Start);
            Get (Line (Start + 1 .. Line'Last), Robot.V.Y, Start);

            return Input_Line (Result & Robot);
         end;
      exception
         when End_Error =>
            return Result;
      end Input_Line;
   begin
      return Input_Line (Robot_Array'[]);
   end Input;

   procedure Simulate (Robots : in out Robot_Array; Time : Positive) is
   begin
      for I in Robots'Range loop
         Robots (I).P.X :=
           X_Position'Mod (Integer (@) + Robots (I).V.X * Time);
         Robots (I).P.Y :=
           Y_Position'Mod (Integer (@) + Robots (I).V.Y * Time);
      end loop;
   end Simulate;
end Advent.Day_14;
