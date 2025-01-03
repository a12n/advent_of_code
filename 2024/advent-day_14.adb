with Ada.Integer_Text_IO;

package body Advent.Day_14 is
   function Distance (P, Q : Position) return Natural is
   begin
      return
        abs (Integer (P.X) - Integer (Q.X)) +
        abs (Integer (P.Y) - Integer (Q.Y));
   end Distance;

   function Distance (Robots : Robot_Array) return Natural is
      Total : Natural := 0;
   begin
      for I in Robots'First .. Robots'Last - 1 loop
         for J in I + 1 .. Robots'Last loop
            Total := Total + Distance (Robots (I).P, Robots (J).P);
         end loop;
      end loop;
      return Total;
   end Distance;

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
            for Char of Line loop
               case Char is
                  when '-' | '0' .. '9' =>
                     null;
                  when others =>
                     Char := ' ';
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

   procedure Print (File : File_Type; Robots : Robot_Array) is
      Lines :
        array (Y_Position) of String (1 .. Positive (X_Position'Last) + 1) :=
        [others => [others => ' ']];
   begin
      for I in Robots'Range loop
         Lines (Robots (I).P.Y) (Integer (Robots (I).P.X) + 1) := '#';
      end loop;
      for I in Lines'Range loop
         Put_Line (File, Lines (I));
      end loop;
   end Print;

   procedure Simulate (Robots : in out Robot_Array; Time : Positive) is
   begin
      for Robot of Robots loop
         Robot.P.X := X_Position'Mod (Integer (@) + Robot.V.X * Time);
         Robot.P.Y := Y_Position'Mod (Integer (@) + Robot.V.Y * Time);
      end loop;
   end Simulate;
end Advent.Day_14;
