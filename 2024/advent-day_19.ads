with Ada.Containers.Hashed_Maps;
with Ada.Strings.Bounded.Hash;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_19 is
   package Bounded_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (60);
   use Bounded_Strings;

   function Bounded_Strings_Hash is new Ada.Strings.Bounded.Hash
     (Bounded_Strings);
   package Bounded_Strings_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Bounded_String, Element_Type => Natural,
      Hash     => Bounded_Strings_Hash, Equivalent_Keys => "=");

   type Design_Type is new Bounded_String;
   type Towel_Type is new Bounded_String;
   type Towel_Array is array (Positive range <>) of Towel_Type;
   subtype Towel_Cache is Bounded_Strings_Maps.Map;

   function Design_Possible
     (Towels : Towel_Array; Design : Design_Type) return Boolean;
   function Get_Design (File : File_Type) return Design_Type;
   function Get_Towels (File : File_Type) return Towel_Array;
   function Number_Arrangements
     (Cache : in out Towel_Cache; Towels : Towel_Array; Design : Design_Type)
      return Natural;
end Advent.Day_19;
