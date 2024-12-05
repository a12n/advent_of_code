with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;

package body Advent.Day_05 is
   function In_Order (Order : Precedence; Pages : Page_Array) return Boolean is
   begin
      for I in Pages'First .. Pages'Last - 1 loop
         for J in I + 1 .. Pages'Last loop
            Put_Line (Standard_Error, Pages (I)'Image & Pages (J)'Image & ' ' & Order (Pages (I), Pages (J))'Image);
            if Order (Pages (I), Pages (J)) = False then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end In_Order;

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
      Result : Precedence := [others => [others => Unknown]];
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
            if Result (A, B) = True then
               Result (B, A) := False;
               for C in Result'Range (2) loop
                  if Result (B, C) = True then
                     Result (A, C) := True;
                     Result (C, A) := False;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;

      return Result;
   end Input_Precedence;

   procedure Put_Image (Buffer : in out Root_Buffer_Type'Class; Order : in Precedence) is
   begin
      for I in Order'Range (1) loop
         for J in Order'Range (2) loop
            case Order (I, J) is
               when Unknown => Buffer.Put ("_");
               when False => Buffer.Put ("F");
               when True => Buffer.Put ("T");
            end case;
         end loop;
         Buffer.Put ("" & ASCII.LF);
      end loop;
   end Put_Image;
end Advent.Day_05;
