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

   function Difference (Levels : Level_Array) return Difference_Array is
   begin
      return Result : Difference_Array (Levels'First .. Levels'Last - 1) do
         for I in Levels'First .. Levels'Last - 1 loop
            Result (I) := Levels (I + 1) - Levels (I);
         end loop;
      end return;
   end Difference;

   function Input_Entry (File : File_Type) return Level_Array is
      Buffer : Level_Vectors.Vector;
   begin
      Get_Input_Entry (Buffer);
      return Result : Level_Array (Buffer.First_Index .. Buffer.Last_Index) do
         for I in Buffer.First_Index .. Buffer.Last_Index loop
            Result (I) := Buffer.Element (I);
         end loop;
      end return;
   end Input_Entry;

   function Is_Monotone (Difference : Difference_Array;
                         Min, Max : Integer;
                         Dampener : Boolean) return Boolean is
      function Within_Range (Difference : Integer) return Boolean is
         (Difference >= Min and Difference <= Max);

      Dampened : Boolean := False;
   begin
      for I in Difference'Range loop
         if not Within_Range (Difference (I)) then
            if not Dampener or Dampened then
               return False;
            end if;

            if I = Difference'First or I = Difference'Last then
               Dampened := True;
            elsif I > Difference'First and Within_Range (Difference (I - 1) + Difference (I)) then
               Dampened := True;
            elsif I < Difference'Last and Within_Range (Difference (I) + Difference (I + 1)) then
               Dampened := True;
               --  I := I + 1;
            else
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Monotone;

   function Is_Monotone (Levels   : Level_Vectors.Vector;
                         Min, Max : Integer;
                         Dampener : Boolean) return Boolean is
      function In_Order (I, J : Positive) return Boolean is
         Distance : constant Integer := Levels.Element (J) - Levels.Element (I);
      begin
         return Distance >= Min and Distance <= Max;
      end In_Order;

      Dampened : Boolean := False;
   begin
      for I in Levels.First_Index + 1 .. Levels.Last_Index loop
         if not In_Order (I, I - 1) then
            if Dampener and not Dampened then
               Dampened := True;
               if I - 1 > Levels.First_Index then
                  if not In_Order (I, I - 2) then
                     return False;
                  end if;
               end if;
            else
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Monotone;
end Advent.Day_02;
