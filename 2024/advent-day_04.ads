with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_04 is
   type Offset is array (1 .. 2) of Integer with
     Dynamic_Predicate => Offset (1) /= 0 or Offset (2) /= 0;
   type Position is array (1 .. 2) of Integer;

   --  function axpy (P : Position; V : Offset; N : Integer) return Position;
   --  function "*" (V : Offset; N : Integer) return Offset;
   function "+" (P : Position; V : Offset) return Position;

   type Word_Search is
     array (Positive range <>, Positive range <>) of Character;

   function Has_Word
     (Letters : Word_Search; Word : String; Origin : Position; Dir : Offset)
      return Boolean with
     Pre => Word'Length > 0;

   function Input (File : File_Type) return Word_Search;

   function Is_X_MAS (Letters : Word_Search; Pos : Position) return Boolean;

   function Num_Words
     (Letters : Word_Search; Word : String; Origin : Position)
      return Natural with
     Pre => Word'Length > 0;
end Advent.Day_04;
