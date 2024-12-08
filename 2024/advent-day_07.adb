with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Advent.Day_07 is
   function Input_Entry
     (File : in File_Type; Test : out Number) return Number_Array
   is
      Line : constant String := Get_Line (File);
      Last : Positive;

      function Input_Numbers (Line : String) return Number_Array is
      begin
         return Number_Array'[];
      end Input_Numbers;
   begin
      Get (Line, Test, Last);
      if Line (Last + 1) /= ':' then
         raise Constraint_Error;
      end if;
      return Input_Numbers (Line (Last + 2 .. Line'Last));
   end Input_Entry;
end Advent.Day_07;
