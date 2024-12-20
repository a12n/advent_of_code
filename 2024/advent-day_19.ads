with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Bounded.Hash;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package Advent.Day_19 is
   type Stripe_Type is (White, Blue, Black, Red, Green);
   type Stripe_Array is array (Positive range <>) of Stripe_Type;
   subtype Design_Type is Stripe_Array;
   type Towel_Set is private;

   function Design_Possible
     (Towels : Towel_Set; Design : Design_Type) return Boolean;
   function Get_Design (File : File_Type) return Design_Type;
   function Get_Towels (File : File_Type) return Towel_Set;
   function Number_Arrangements
     (Towels : Towel_Set; Design : Design_Type) return Natural;

private
   package Towel_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Stripe_Array);

   type Towel_Set is record
      Towels : Towel_Vectors.Vector;
   end record;

   --  type Node;
   --  type Node_Access is access Node;
   --  type Node_Links is array (Stripe_Type) of Node_Access;
   --
   --  type Node is record
   --     Valid : Boolean;
   --     Next  : Node_Links;
   --  end record;
   --
   --  procedure Copy (From_Trie : Node_Access; To_Trie : out Node_Access);
   --  procedure Deallocate (Trie : in out Node_Access);
   --  procedure Deallocate_Node is new Ada.Unchecked_Deallocation
   --    (Node, Node_Access);
   --  procedure Include (Trie : in out Node_Access; Stripes : Design_Type);
   --
   --  type Towel_Set is record
   --     Root : Node_Access;
   --  end record;

end Advent.Day_19;
