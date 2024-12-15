with Ada.Text_IO;  use Ada.Text_IO;
with Advent.Grids; use Advent.Grids;

package Advent.Day_06 is
   type Blocked_Map is array (Positive range <>, Positive range <>) of Boolean;
   type Visited_Map is
     array
       (Positive range <>, Positive range <>, Direction range <>) of Boolean;

   function Input
     (File : File_Type; Guard_Pos : out Position; Guard_Dir : out Direction)
      return Blocked_Map;

   Loop_Error : exception;

   procedure Walk
     (Blocked :    Blocked_Map; Guard_Pos : Position; Guard_Dir : Direction;
      Visited : in out Visited_Map);

   procedure Walk
     (Blocked :    Blocked_Map; Guard_Pos : Position; Guard_Dir : Direction;
      Visited : in out Visited_Map; Number_Loops : out Natural);
end Advent.Day_06;
