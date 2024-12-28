package body Advent.Day_24 is
   procedure Iterate
     (Wires   : Wire_Map; ID : Character;
      Process : not null access procedure (Position : Wire_Maps.Cursor))
   is
      use Wire_Maps;
      use type Wire_Maps.Cursor;

      I : Cursor := Wires.Find (ID & "00");
   begin
      if I = No_Element then
         raise Constraint_Error with "No wires for " & ID'Image;
      end if;

      while I /= No_Element and then Key (I) (1) = ID loop
         Process (I);
         I := Next (I);
      end loop;
   end Iterate;

   function Get_Wires (File : File_Type) return Wire_Map is
      Wires : Wire_Map;

      procedure Swap is new Generic_Swap (Wire_Name);

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

         if Wire.B < Wire.A then
            Swap (Wire.A, Wire.B);
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

   function Number (Wires : Wire_Map; ID : Character) return Number_Type is
      use Wire_Maps;
      N : Number_Type := 0;
      K : Number_Type := 1;                --  2^0

      procedure Process (I : Cursor) is
      begin
         if Signal (Wires, I.Key) then
            N := N or K;
         end if;

         if Debug_Level > 1 then
            Put_Line
              (Standard_Error,
               "I " & I.Key'Image & ", K " & K'Image & ", N " & N'Image);
         end if;

         K := K * 2;
      end Process;
   begin
      Iterate (Wires, ID, Process'Access);
      return N;
   end Number;

   function Signal (Wires : Wire_Map; Name : Wire_Name) return Boolean is
      Wire : constant Wire_Type := Wires.Element (Name);
   begin
      if Debug_Level > 1 then
         Put_Line
           (Standard_Error, "Name " & Name'Image & ", Wire " & Wire'Image);
      end if;

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

   function Full_Adder
     (Wires : Wire_Map; A, B, S, C_out : Wire_Name; C_in : out Wire_Name)
      return Boolean
   is
      A_Wire     : constant Wire_Type := Wires.Element (A);
      B_Wire     : constant Wire_Type := Wires.Element (B);
      S_Wire     : constant Wire_Type := Wires.Element (S);
      C_out_Wire : constant Wire_Type := Wires.Element (C_out);
   begin
      if A_Wire.Gate not in '0' .. '1' or B_Wire.Gate not in '0' .. '1' then
         --  Input wires for X and Y bits must be constants.
         return False;
      end if;

      if S_Wire.Gate /= '^' then
         --  Sum output is from XOR gate.
         return False;
      end if;

      if C_out_Wire.Gate /= '|' then
         --  Output carry bit is from OR gate.
         return False;
      end if;

      --  TODO
      return False;
   end Full_Adder;

   function Half_Adder
     (Wires : Wire_Map; A, B, S, C_out : Wire_Name) return Boolean
   is
      A_Wire     : constant Wire_Type := Wires.Element (A);
      B_Wire     : constant Wire_Type := Wires.Element (B);
      S_Wire     : constant Wire_Type := Wires.Element (S);
      C_out_Wire : constant Wire_Type := Wires.Element (C_out);
   begin
      return
        A_Wire.Gate in '0' .. '1' and B_Wire.Gate in '0' .. '1' and
        (S_Wire.Gate = '^' and S_Wire.A = A and S_Wire.B = B) and
        (C_out_Wire.Gate = '&' and C_out_Wire.A = A and C_out_Wire.B = B);
   end Half_Adder;
end Advent.Day_24;
