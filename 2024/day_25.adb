with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;

procedure Day_25 is
   --  Grid of 7×5 bits may be represented as 35-bit number.
   --  type Schematic_Type is mod 2**35;
   --  type Schematic_Array is array (Positive range <>) of Schematic_Type;
   --
   --  package Schematic_Text_IO is new Ada.Text_IO.Modular_IO (Schematic_Type);
   --
   --  subtype Key_Type is Schematic_Type with
   --      Dynamic_Predicate => Key (Key_Type);
   --  subtype Lock_Type is Schematic_Type with
   --      Dynamic_Predicate => Lock (Lock_Type);
   --
   --  function Key (S : Schematic_Type) return Boolean is
   --    ((S and 2#1_1111#) = 2#1_1111#);
   --  function Lock (S : Schematic_Type) return Boolean is
   --    ((S and 2#1_1111#) = 2#0_0000#);
   --  function Fit (K : Key_Type; L : Lock_Type) return Boolean is
   --    ((K xor L) = -1);

   type Pin_Height is range 0 .. 5;
   type Pin_Array is array (1 .. 5) of Pin_Height;
   type Schematic_Type is record
      Key  : Boolean;
      Pins : Pin_Array;
   end record;
   type Schematic_Array is array (Positive range <>) of Schematic_Type;

   function Fit (S, T : Schematic_Type) return Boolean is
     ((S.Key xor T.Key) and
      (for all I in 1 .. 5 => (S.Pins (I) + T.Pins (I)) in Pin_Height'Range));

   function Get_Schematic (File : File_Type) return Schematic_Type is
      C : Character;
      S : Schematic_Type := (False, [others => 0]);
   begin
      declare
         Line : constant String := Get_Line (File);
      begin
         case Line is
            when "....." =>
               S.Key := True;
            when "#####" =>
               S.Key := False;
            when others =>
               raise Constraint_Error;
         end case;
      end;

      for Row in 1 .. 5 loop
         declare
            Line : constant String := Get_Line (File);
         begin
            for Col in 1 .. 5 loop
               case Line (Col) is
                  when '.' =>
                     null;
                  when '#' =>
                     S.Pins (Col) := @ + 1;
                  when others =>
                     raise Constraint_Error;
               end case;
            end loop;
         end;
      end loop;

      declare
         Line : constant String := Get_Line (File);
      begin
         case Line is
            when "....." =>
               if S.Key then
                  raise Constraint_Error;
               end if;
            when "#####" =>
               if not S.Key then
                  raise Constraint_Error;
               end if;
            when others =>
               raise Constraint_Error;
         end case;
      end;

      --  for I in 0 .. 35 - 1 loop
      --     Get (File, C);
      --     case C is
      --        when '.' =>
      --           S := S * 2;
      --        when '#' =>
      --           S := S * 2 or 1;
      --        when others =>
      --           raise Constraint_Error;
      --     end case;
      --  end loop;

      begin
         Skip_Line (File);
      exception
         when End_Error =>
            null;
      end;

      return S;
   end Get_Schematic;

   function Get_Schematics (File : File_Type) return Schematic_Array is
      Buffer : Schematic_Array (1 .. 500 + 1);
      Offset : Positive := Buffer'First;
   begin
      loop
         Buffer (Offset) := Get_Schematic (File);
         Offset          := Offset + 1;
      end loop;
   exception
      when End_Error =>
         return Buffer (1 .. Offset - 1);
   end Get_Schematics;

   N          : Natural                  := 0;
   Schematics : constant Schematic_Array := Get_Schematics (Standard_Input);
begin
   for I in Schematics'First .. Schematics'Last - 1 loop
      for J in I + 1 .. Schematics'Last loop
         if Fit (Schematics (I), Schematics (J)) then
            N := N + 1;
         end if;
      end loop;
   end loop;

   Put (N, 0);
   New_Line;
end Day_25;
