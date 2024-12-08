with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_07; use Advent.Day_07;

procedure Day_07_1 is
   use Number_Text_IO;

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

   Total : Number := 0;
begin
   loop
      declare
         Test    : Number;
         Numbers : constant Number_Array := Input_Entry (Standard_Input, Test);
      begin
         if Valid (Test, Numbers) then
            Total := Total + Test;
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_07_1;
