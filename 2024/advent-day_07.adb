with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Advent.Day_07 is
   use Number_Text_IO;

   function Input_Entry
     (File : in File_Type; Test : out Number) return Number_Array
   is
      Line  : constant String  := Get_Line (File);
      Colon : constant Natural := Index (Line, ":", 1);
      Last  : Positive;

      function Input_Numbers (Line : String) return Number_Array is
         N : Number;
      begin
         if Line'Length = 0 then
            return Number_Array'[];
         end if;
         Get (Line, N, Last);
         return
           Number_Array'[N] & Input_Numbers (Line (Last + 1 .. Line'Last));
      end Input_Numbers;
   begin
      Get (Line (Line'First .. Colon - 1), Test, Last);
      return Input_Numbers (Line (Colon + 1 .. Line'Last));
   end Input_Entry;

   function Valid (Test : Number; Operands : Number_Array) return Boolean is
   begin
      case Operands'Length is
         when 0 =>
            return False;
         when 1 =>
            return Test = Operands (Operands'First);
         when others =>
            if Operands (Operands'First) > Test then
               return False;
            end if;
            declare
               A : Number renames Operands (Operands'First);
               B : Number renames Operands (Operands'First + 1);
               S : constant Number := A + B;
               P : constant Number := A * B;
            begin
               return
                 Valid
                   (Test,
                    Number_Array'[S] &
                    Operands (Operands'First + 2 .. Operands'Last))
                 or else Valid
                   (Test,
                    Number_Array'[P] &
                    Operands (Operands'First + 2 .. Operands'Last));
            end;
      end case;
   end Valid;
end Advent.Day_07;
