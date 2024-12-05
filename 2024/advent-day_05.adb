with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;

package body Advent.Day_05 is
   function Input_Pages (File : File_Type) return Page_Array is
      Line : constant String := Get_Line (File);
      N : constant Positive := Ada.Strings.Fixed.Count (Line, To_Set (',')) + 1;
      Pages : Page_Array (1 .. N);
      Start : Positive := Line'First;
   begin
      for I in Pages'Range loop
         Get (Line (Start .. Line'Last), Pages (I), Start);
         Start := Start + 2;    --  Skip past ',' where the next number starts.
      end loop;
      return Pages;
   end Input_Pages;

   function Input_Precedence (File : File_Type) return Precedence is
      Result : Precedence := [others => [others => False]];
   begin
      loop
         declare
            Line : constant String := Get_Line (File);
            Before, After : Page_Number;
            Stop : Positive;
         begin
            exit when Line'Length = 0;
            Get (Line, Before, Stop);
            Get (Line (Stop + 2 .. Line'Last), After, Stop);
            Result (Before, After) := True;
         end;
      end loop;

      --  If page A goes before B and B goes before C, then A goes
      --  before C.
      for A in Result'Range (1) loop
         for B in Result'Range (2) loop
            if Result (A, B) then
               for C in Result'Range (2) loop
                  if Result (B, C) then
                     Result (A, C) := True;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;

      return Result;
   end Input_Precedence;
end Advent.Day_05;
