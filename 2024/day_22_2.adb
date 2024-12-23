with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_22;       use Advent.Day_22;
with Advent;              use Advent;

procedure Day_22_2 is
   use Number_Text_IO;

   type Digit_Type is range 0 .. 9;
   type Digit_Index is mod 2;
   type Digit_Sequence is array (Digit_Index) of Digit_Type;

   type Change_Type is range -9 .. 9;
   type Change_Index is mod 4;
   type Change_Sequence is array (Change_Index) of Change_Type;

   type Count_Array is
     array (Change_Type, Change_Type, Change_Type, Change_Type) of Natural;

   Current : Number_Type;

   --  Number of bananas for each possible sequence of four
   --  consecutive changes.
   Bananas : Count_Array := [others => [others => [others => [others => 0]]]];
begin
   --  TODO
   loop
      Get (Standard_Input, Current);
      declare
         Values  : Digit_Sequence  := [others => 0];
         Changes : Change_Sequence := [others => 0];
      begin
         for I in 0 .. 2_000 - 1 loop
            Values (Digit_Index'Mod (I)) := Digit_Type (Current mod 10);

            if I > 0 then
               Changes (Change_Index'Mod (I)) :=
                 Change_Type (Values (Digit_Index'Mod (I))) -
                 Change_Type (Values (Digit_Index'Mod (I - 1)));
            end if;

            if I > Natural (Change_Index'Last) then
               Bananas
                 (Changes (Change_Index'Mod (I - 3)),
                  Changes (Change_Index'Mod (I - 2)),
                  Changes (Change_Index'Mod (I - 1)),
                  Changes (Change_Index'Mod (I))) :=
                 @ + 1;
            end if;

            Current := Evolve (Current);
         end loop;
      end;
   end loop;

exception
   when End_Error =>
      declare
         Max_Bananas : Natural := 0;
      begin
         for A in Change_Type'Range loop
            for B in Change_Type'Range loop
               for C in Change_Type'Range loop
                  for D in Change_Type'Range loop
                     if Bananas (A, B, C, D) > Max_Bananas then
                        Max_Bananas := Bananas (A, B, C, D);
                        if Debug then
                           Put_Line
                             (Standard_Error,
                              "Sequence " & A'Image & ", " & B'Image & ", " &
                              C'Image & ", " & D'Image & ", bananas " &
                              Max_Bananas'Image);
                        end if;
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;
         Put (Max_Bananas, 0);
         New_Line;
      end;
end Day_22_2;
