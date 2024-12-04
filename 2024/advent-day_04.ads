with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_04 is
   subtype Sign is Integer range -1..1;
   type Direction_Type is array (1 .. 2) of Sign
     with Dynamic_Predicate => Direction_Type (1) /= 0 or Direction_Type (2) /= 0;

   type Word_Search is array (Positive range <>, Positive range <>) of Character;

   function Input (File : File_Type) return Word_Search;
   function Has_Word (Letters : Word_Search;
                      Word : String;
                      Row, Col : Positive;
                      Direction : Direction_Type) return Boolean;
   function Num_Words (Letters : Word_Search;
                       Word : String;
                       Row, Col : Positive) return Natural;
end Advent.Day_04;
