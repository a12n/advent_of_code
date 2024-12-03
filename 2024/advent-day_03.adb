package body Advent.Day_03 is
   procedure Get_Input_Entry (File : in File_Type; N, M : out Natural) is
      Next  : Character;
      State : Natural := 0;
   begin
      N := 0;
      M := 0;
      loop
         Get (File, Next);
         case Next is
            when 'm' =>
               case State is
                  when 0 => State := 1;
                  when others => State := 0;
               end case;
            when 'u' =>
               case State is
                  when 1 => State := 2;
                  when others => State := 0;
               end case;
            when 'l' =>
               case State is
                  when 2 => State := 3;
                  when others => State := 0;
               end case;
            when '(' =>
               case State is
                  when 3 =>
                     N := 0;
                     State := 4;
                  when others => State := 0;
               end case;
            when '0' .. '9' =>
               case State is
                  when 4 => N := N * 10 + Natural (Character'Pos (Next) - Character'Pos ('0'));
                  when 5 => M := M * 10 + Natural (Character'Pos (Next) - Character'Pos ('0'));
                  when others => State := 0;
               end case;
            when ',' =>
               case State is
                  when 4 =>
                     M := 0;
                     State := 5;
                  when others => State := 0;
               end case;
            when ')' =>
               case State is
                  when 5 => return;
                  when others => State := 0;
               end case;
            when others =>
               State := 0;
         end case;
      end loop;
   end Get_Input_Entry;
end Advent.Day_03;
