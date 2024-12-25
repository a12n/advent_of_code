with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;

procedure Day_25 is
   --  Grid of 7×5 bits may be represented as 35-bit number.
   type Schematic_Type is mod 2**35;
   type Schematic_Array is array (Positive range <>) of Schematic_Type;

   package Schematic_Text_IO is new Ada.Text_IO.Modular_IO (Schematic_Type);

   subtype Key_Type is Schematic_Type with
       Dynamic_Predicate => Key (Key_Type);
   subtype Lock_Type is Schematic_Type with
       Dynamic_Predicate => Lock (Lock_Type);

   function Key (S : Schematic_Type) return Boolean is
     ((S and 2#1_1111#) = 2#1_1111#);
   function Lock (S : Schematic_Type) return Boolean is
     ((S and 2#1_1111#) = 2#0_0000#);
   function Fit (K : Key_Type; L : Lock_Type) return Boolean is
     ((K xor L) = -1);

   function Get_Schematic (File : File_Type) return Schematic_Type is
      C : Character;
      S : Schematic_Type := 0;
   begin
      for I in 0 .. 35 - 1 loop
         Get (File, C);
         case C is
            when '.' =>
               S := S * 2;
            when '#' =>
               S := S * 2 or 1;
            when others =>
               raise Constraint_Error;
         end case;
      end loop;
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
   for S of Schematics loop
      Put
        (Standard_Error,
         (if Key (S) then "K " elsif Lock (S) then "L " else "? "));
      Schematic_Text_IO.Put (Standard_Error, S, Width => 40, Base => 2);
      New_Line (Standard_Error);
   end loop;

   for I in Schematics'First .. Schematics'Last - 1 loop
      Put (Standard_Error, "I ");
      Schematic_Text_IO.Put
        (Standard_Error, Schematics (I), Width => 40, Base => 2);
      New_Line (Standard_Error);

      for J in I + 1 .. Schematics'Last loop
         Put (Standard_Error, "J ");
         Schematic_Text_IO.Put
           (Standard_Error, Schematics (J), Width => 40, Base => 2);
         New_Line (Standard_Error);

         Put (Standard_Error, "^ ");
         Schematic_Text_IO.Put
           (Standard_Error, Schematics (I) xor Schematics (J), Width => 40,
            Base                                                     => 2);
         New_Line (Standard_Error);

         if (Schematics (I) xor Schematics (J)) = -1 then
            N := N + 1;
         end if;
      end loop;
   end loop;

   Put (N, 0);
   New_Line;
end Day_25;
