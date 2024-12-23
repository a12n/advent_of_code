with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_23;       use Advent.Day_23;

procedure Day_23_2 is
   Online    : Address_Set;
   Connected : constant Connection_Map :=
     Get_Connections (Standard_Input, Online);

   Max_Clique : Address_Set := [others => False];
   Max_Length : Natural     := 0;

   procedure Bron_Kerbosch (R, P, X : in out Address_Set) is
   begin
      if Empty (P) and Empty (X) then
         declare
            N : constant Natural := Length (R);
         begin
            --  Put_Line
            --    (Standard_Error,
            --     "maximal clique" & R'Image & ", length " & N'Image);
            if N > Max_Length then
               Max_Clique := R;
               Max_Length := N;
            end if;
         end;
      end if;

      for I in P'Range loop
         if P (I) then
            declare
               R2 : Address_Set := R;
               P2 : Address_Set := P and Connected (I);
               X2 : Address_Set := X and Connected (I);
            begin
               R2 (I) := True;
               Bron_Kerbosch (R => R2, P => P2, X => X2);
               P (I) := False;
               X (I) := True;
            end;
         end if;
      end loop;
   end Bron_Kerbosch;
begin
   --  TODO: Maximum clique.
   Put_Line (Standard_Error, "Online" & Online'Image);
   Put_Line (Standard_Error, "Connected" & Connected'Image);
   declare
      R : Address_Set := [others => False];
      X : Address_Set := [others => False];
      P : Address_Set := Online;
   begin
      Bron_Kerbosch (R => R, X => X, P => P);
   end;

   Put_Line (To_String (Max_Clique));
end Day_23_2;
