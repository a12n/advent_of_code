with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;    use Ada.Strings.Maps;

package body Advent.Day_05 is
   function In_Order (Before : Precedence; Pages : Page_Array) return Boolean
   is
   begin
      for I in Pages'First .. Pages'Last - 1 loop
         for J in I + 1 .. Pages'Last loop
            if not Before (Pages (I), Pages (J)) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end In_Order;

   function Input_Pages (File : File_Type) return Page_Array is
      Line  : constant String   := Get_Line (File);
      N     : constant Positive :=
        Ada.Strings.Fixed.Count (Line, To_Set (',')) + 1;
      Pages : Page_Array (1 .. N);
      Start : Positive          := Line'First;
   begin
      for I in Pages'Range loop
         Get (Line (Start .. Line'Last), Pages (I), Start);
         Start := Start + 2;    --  Skip past ',' where the next number starts.
      end loop;
      return Pages;
   end Input_Pages;

   function Input_Precedence (File : File_Type) return Precedence is
      Before : Precedence := [others => [others => False]];
   begin
      loop
         declare
            Line : constant String := Get_Line (File);
            A, B : Page_Number;
            Stop : Positive;
         begin
            exit when Line'Length = 0;
            Get (Line, A, Stop);
            Get (Line (Stop + 2 .. Line'Last), B, Stop);
            --  if Before (A, B) = False or Before (B, A) = True then
            --     raise Constraint_Error;
            --  end if;
            Before (A, B) := True;
            Before (B, A) := False;
         end;
      end loop;
      return Before;
   end Input_Precedence;
end Advent.Day_05;
