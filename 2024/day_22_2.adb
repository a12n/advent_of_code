with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_22;             use Advent.Day_22;
with Advent.Debug;              use Advent.Debug;

procedure Day_22_2 is
   Max_Numbers : constant Positive :=
     Positive'Value (Value ("MAX_NUMBERS", "2000"));

   use Number_Text_IO;

   type Price_Type is range 0 .. 9;
   type Price_Index is mod 2;
   type Price_Sequence is array (Price_Index) of Price_Type;

   type Change_Type is range -9 .. 9;
   type Change_Index is mod 4;
   type Change_Sequence is array (Change_Index) of Change_Type;

   type Change_Set is
     array (Change_Type, Change_Type, Change_Type, Change_Type) of Boolean;
   type Change_Count is
     array (Change_Type, Change_Type, Change_Type, Change_Type) of Natural;
   pragma Pack (Change_Set);

   --  Number of bananas for each possible sequence of four
   --  consecutive changes.
   Total_Bananas : Change_Count :=
     [others => [others => [others => [others => 0]]]];
begin
   loop
      declare
         --  Per buyer variables.
         N       : Number_Type;
         Prices  : Price_Sequence  := [others => 0];
         Changes : Change_Sequence := [others => 0];

         Buyer_Seen_Changes : Change_Set :=
           [others => [others => [others => [others => False]]]];
      begin
         Get (Standard_Input, N);
         if Debug_Enabled then
            Put_Line (Standard_Error, "Buyer " & N'Image);
         end if;

         --  Evolve buyer numbers.
         for I in 0 .. Max_Numbers - 1 loop
            declare
               --  Last two prices.
               Price   : Price_Type renames Prices (Price_Index'Mod (I));
               Price_1 : Price_Type renames Prices (Price_Index'Mod (I - 1));

               --  Last four price changes.
               Change   : Change_Type renames Changes (Change_Index'Mod (I));
               Change_1 :
                 Change_Type renames Changes (Change_Index'Mod (I - 1));
               Change_2 :
                 Change_Type renames Changes (Change_Index'Mod (I - 2));
               Change_3 :
                 Change_Type renames Changes (Change_Index'Mod (I - 3));
            begin
               --  Derive price from the buyer random number.
               Price := Price_Type (N mod 10);
               if Debug_Enabled then
                  New_Line (Standard_Error);
                  Put_Line
                    (Standard_Error,
                     "Price " & Price'Image & " (" & N'Image & ")");
               end if;

               --  There are at least two prices, can compute change sequence now.
               if I > Natural (Price_Index'First) then
                  Change := Change_Type (Price) - Change_Type (Price_1);
                  if Debug_Enabled then
                     Put_Line (Standard_Error, "Change " & Change'Image);
                  end if;
               end if;

               --  There are at least four price changes, count prices
               --  for change sequences.
               if I > Natural (Change_Index'Last) then
                  declare
                     Bananas      :
                       Natural renames
                       Total_Bananas (Change_3, Change_2, Change_1, Change);
                     Seen_Changes :
                       Boolean renames
                       Buyer_Seen_Changes
                         (Change_3, Change_2, Change_1, Change);
                  begin
                     if Debug_Enabled then
                        Put_Line
                          (Standard_Error,
                           "Changes " & Change_3'Image & ", " &
                           Change_2'Image & ", " & Change_1'Image & ", " &
                           Change'Image & ", seen " & Seen_Changes'Image);
                     end if;

                     if not Seen_Changes then
                        Bananas      := @ + Natural (Price);
                        Seen_Changes := True;
                        if Debug_Enabled then
                           Put_Line
                             (Standard_Error,
                              "Bananas + " & Price'Image & " = " &
                              Bananas'Image);
                        end if;
                     end if;
                  end;
               end if;

               N := Evolve (N);
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
                     if Total_Bananas (A, B, C, D) > Max_Bananas then
                        Max_Bananas := Total_Bananas (A, B, C, D);
                        if Debug_Enabled then
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
