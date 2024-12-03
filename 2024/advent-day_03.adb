package body Advent.Day_03 is
   procedure Get_Input_Entry (File    : in File_Type;
                              N, M    : out Natural;
                              Enabled : in out Boolean) is
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
                  when 11 => State := 12;
                  when 22 => State := 23;
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
                  when 12 =>
                     Enabled := True;
                     State := 0;
                  when 23 =>
                     Enabled := False;
                     State := 0;
                  when others => State := 0;
               end case;
            when 'd' =>
               case State is
                  when 0 => State := 10;
                  when others => State := 0;
               end case;
            when 'o' =>
               case State is
                  when 10 => State := 11;
                  when others => State := 0;
               end case;
            when 'n' =>
               case State is
                  when 11 => State := 20;
                  when others => State := 0;
               end case;
            when ''' =>
               case State is
                  when 20 => State := 21;
                  when others => State := 0;
               end case;
            when 't' =>
               case State is
                  when 21 => State := 22;
                  when others => State := 0;
               end case;
            when others =>
               State := 0;
         end case;
      end loop;
   end Get_Input_Entry;
end Advent.Day_03;
