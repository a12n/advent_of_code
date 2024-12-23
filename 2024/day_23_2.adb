with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_23;       use Advent.Day_23;

procedure Day_23_2 is
   Online    : Address_Set;
   Connected : constant Connection_Map :=
     Get_Connections (Standard_Input, Online);

   Max_Clique : Address_Set := [others => False];
   Max_Length : Natural     := 0;

   procedure Bron_Kerbosch (Complete, Candidates, Exclude : in out Address_Set)
   is
   begin
      if Empty (Candidates) and Empty (Exclude) then
         declare
            N : constant Natural := Length (Complete);
         begin
            if N > Max_Length then
               Max_Clique := Complete;
               Max_Length := N;
            end if;
         end;
      end if;

      for I in Address'Range loop
         if Candidates (I) then
            declare
               Next_Complete   : Address_Set := Complete;
               Next_Candidates : Address_Set := Candidates and Connected (I);
               Next_Exclude    : Address_Set := Exclude and Connected (I);
            begin
               Next_Complete (I) := True;
               Bron_Kerbosch
                 (Complete => Next_Complete, Candidates => Next_Candidates,
                  Exclude  => Next_Exclude);
               Candidates (I) := False;
               Exclude (I)    := True;
            end;
         end if;
      end loop;
   end Bron_Kerbosch;
begin
   declare
      Complete   : Address_Set := [others => False];
      Candidates : Address_Set := Online;
      Exclude    : Address_Set := [others => False];
   begin
      Bron_Kerbosch
        (Complete => Complete, Candidates => Candidates, Exclude => Exclude);
   end;

   Put_Line (To_String (Max_Clique));
end Day_23_2;
