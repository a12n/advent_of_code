with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_22;             use Advent.Day_22;
with Advent;                    use Advent;

procedure Day_22_2 is
   use Number_Text_IO;

   Max_Numbers : constant Positive :=
     Positive'Value (Value ("MAX_NUMBERS", "2000"));

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

         --  Whether already sold to this buyer on this sequence.
         Seen_Sequences : Count_Array :=
           [others => [others => [others => [others => 0]]]];
      begin
         if Debug then
            Put_Line (Standard_Error, "Buyer " & Current'Image);
         end if;

         for I in 0 .. Max_Numbers - 1 loop
            declare
               Value   : Digit_Type renames Values (Digit_Index'Mod (I));
               Value_1 : Digit_Type renames Values (Digit_Index'Mod (I - 1));

               Change   : Change_Type renames Changes (Change_Index'Mod (I));
               Change_1 :
                 Change_Type renames Changes (Change_Index'Mod (I - 1));
               Change_2 :
                 Change_Type renames Changes (Change_Index'Mod (I - 2));
               Change_3 :
                 Change_Type renames Changes (Change_Index'Mod (I - 3));
            begin
               Value := Digit_Type (Current mod 10);
               if Debug then
                  New_Line (Standard_Error);
                  Put_Line
                    (Standard_Error,
                     "Value " & Value'Image & " (" & Current'Image & ")");
               end if;

               if I > 0 then
                  Change := Change_Type (Value) - Change_Type (Value_1);
                  if Debug then
                     Put_Line (Standard_Error, "Change " & Change'Image);
                  end if;
               end if;

               if I > Natural (Change_Index'Last) then
                  if Debug then
                     Put_Line
                       (Standard_Error,
                        "Changes " & Change_3'Image & ", " & Change_2'Image &
                        ", " & Change_1'Image & ", " & Change'Image &
                        ", seen " &
                        Seen_Sequences (Change_3, Change_2, Change_1, Change)'
                          Image);
                  end if;

                  if Seen_Sequences (Change_3, Change_2, Change_1, Change) = 0
                  then
                     Bananas (Change_3, Change_2, Change_1, Change)        :=
                       @ + Natural (Value);
                     Seen_Sequences (Change_3, Change_2, Change_1, Change) :=
                       @ + 1;
                     if Debug then
                        Put_Line
                          (Standard_Error,
                           "Bananas + " & Value'Image & " = " &
                           Bananas (Change_3, Change_2, Change_1, Change)'
                             Image);
                     end if;
                  end if;
               end if;

               Current := Evolve (Current);
            end;
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
