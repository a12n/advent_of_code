package body Advent.Day_22 is
   Prune_Modulus : constant := 16_777_216;

   function Evolve (N : Number_Type) return Number_Type is
      M : Number_Type := (N xor (N * 64)) mod Prune_Modulus;
   begin
      M := (M xor (M / 32)) mod Prune_Modulus;
      return (M xor (M * 2_048)) mod Prune_Modulus;
   end Evolve;
end Advent.Day_22;
