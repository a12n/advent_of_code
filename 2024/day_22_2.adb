with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_22;       use Advent.Day_22;

procedure Day_22_2 is
   use Number_Text_IO;

   type Digit_Type is range 0 .. 9;
   type Digit_Sequence is array (1 .. 4) of Digit_Type;

   type Change_Type is range -9 .. 9;
   type Change_Sequence is array (1 .. 4) of Change_Type;

   type Count_Array is
     array (Change_Type, Change_Type, Change_Type, Change_Type) of Natural;
   pragma Pack (Count_Array);

   Current, Previous : Number_Type;

   --  Number of bananas for each possible sequence of four
   --  consecutive changes.
   Bananas : Count_Array := [others => [others => [others => [others => 0]]]];
begin
   --  TODO
   loop
      Get (Standard_Input, Current);
      declare
         Ones    : Digit_Sequence  := [others => 0];
         Changes : Change_Sequence := [others => 0];
      begin
         for I in 1 .. 2_000 loop
            --  Shift Ones
            --  Shift Changes

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
                     Max_Bananas :=
                       Natural'Max (Max_Bananas, Bananas (A, B, C, D));
                  end loop;
               end loop;
            end loop;
         end loop;
         Put (Max_Bananas, 0);
         New_Line;
      end;
end Day_22_2;
