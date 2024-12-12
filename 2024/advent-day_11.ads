with Ada.Containers.Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_11 is
   type Stone_Splitter is limited private;
   type Stone_Type is range 0 .. 2**64;

   package Stone_Text_IO is new Ada.Text_IO.Integer_IO (Stone_Type);

   function Number_Stones
     (Splitter : in out Stone_Splitter; Stone : Stone_Type; Times : Natural)
      return Natural;
private
   --  type State is record
   --     Stone  : Stone_Type;
   --     Blinks : Positive;
   --  end record;
   --
   --  package State_Hashed_Map is new Ada.Containers.Hashed_Maps
   --    (Key_Type => State, Element_Type => Natural);
   --
   --  use State_Hashed_Map;

   type Stone_Splitter is record
      --  Cache : Map := Empty_Map;
      null;
   end record;
end Advent.Day_11;
