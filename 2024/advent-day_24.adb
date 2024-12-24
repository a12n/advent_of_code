package body Advent.Day_24 is
   function Get_Wires (File : File_Type) return Wire_Map is
      Wires : Wire_Map;

      procedure Read_Gate is
         Line : constant String := Get_Line (File);
         Name : Wire_Name;
         Wire : Wire_Type;
      begin
         if Line'Length < 17 then
            raise Constraint_Error with "Invalid gate description line";
         end if;

         if Line'Length = 18 and Line (12 .. 15) = " -> " then
            if Line (4 .. 8) = " AND " then
               Wire.Gate := '&';
            elsif Line (4 .. 8) = " XOR " then
               Wire.Gate := '^';
            else
               raise Constraint_Error;
            end if;
            Name   := Line (16 .. 18);
            Wire.A := Line (1 .. 3);
            Wire.B := Line (9 .. 11);
         elsif Line'Length = 17 and Line (11 .. 14) = " -> " then
            if Line (4 .. 7) = " OR " then
               Wire.Gate := '|';
            else
               raise Constraint_Error;
            end if;
            Name   := Line (15 .. 17);
            Wire.A := Line (1 .. 3);
            Wire.B := Line (8 .. 10);
         else
            raise Constraint_Error;
         end if;

         Wires.Include (Name, Wire);
      end Read_Gate;

      procedure Read_Wire is
         Line : constant String := Get_Line (File);
         Name : Wire_Name;
         Wire : Wire_Type;
      begin
         if Line'Length = 0 then
            raise End_Error;
         end if;

         if Line'Length /= 6 or else Line (4 .. 5) /= ": " then
            raise Constraint_Error with "Invalid wire signal format";
         end if;

         if Line (6) = '0' then
            Wire.Gate := '0';
         elsif Line (6) = '1' then
            Wire.Gate := '1';
         else
            raise Constraint_Error with "Invalid wire signal value";
         end if;
         Name := Line (1 .. 3);

         Wires.Include (Name, Wire);
      end Read_Wire;
   begin
      begin
         loop
            Read_Wire;
         end loop;
      exception
         when End_Error =>
            null;
      end;

      begin
         loop
            Read_Gate;
         end loop;
      exception
         when End_Error =>
            null;
      end;

      return Wires;
   end Get_Wires;

   function Signal (Wires : in out Wire_Map; Name : Wire_Name) return Boolean
   is
      Wire : constant Wire_Type := Wires.Element (Name);
   begin
      case Wire.Gate is
         when '0' =>
            return False;
         when '1' =>
            return True;
         when '&' =>
            return Signal (Wires, Wire.A) and Signal (Wires, Wire.B);
         when '|' =>
            return Signal (Wires, Wire.A) or Signal (Wires, Wire.B);
         when '^' =>
            return Signal (Wires, Wire.A) xor Signal (Wires, Wire.B);
      end case;
   end Signal;
end Advent.Day_24;
