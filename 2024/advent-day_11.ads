with Ada.Containers.Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_11 is
   type Count_Type is range 0 .. 2**64;
   type Stone_Type is range 0 .. 2**64;
   type Stone_Splitter is limited private;

   package Stone_Text_IO is new Ada.Text_IO.Integer_IO (Stone_Type);

   function Number_Stones
     (Splitter : in out Stone_Splitter; Stone : Stone_Type; Blinks : Natural)
      return Count_Type;
private
   type State is record
      Stone  : Stone_Type;
      Blinks : Positive;
   end record;

   function "<" (A, B : State) return Boolean;

   package State_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => State, Element_Type => Count_Type);

   use State_Maps;

   type Stone_Splitter is record
      Cache : Map;
   end record;
end Advent.Day_11;
