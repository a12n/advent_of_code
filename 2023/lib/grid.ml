module Pos = struct
  type t = int * int
  (** Position in a 2D grid. [(0, 0)] is the top-left corner and
    [(n_rows - 1, n_cols - 1)] is the bottom-right corner. *)

  let add (row1, col1) (row2, col2) = (row1 + row2, col1 + col2)
  let mul_int (row, col) n = (row * n, col * n)
  let sub (row1, col1) (row2, col2) = (row1 - row2, col1 - col2)
  let is_valid (n_rows, n_cols) (row, col) = row >= 0 && col >= 0 && row < n_rows && col < n_cols
  let pp fmt (row, col) = Format.fprintf fmt "(%d, %d)" row col
end

type 'a t = 'a array array

let size = Array.matrix_size

let find_pos pred grid =
  Array.(
    find_mapi
      (fun row line ->
        find_mapi (fun col value -> if pred value then Some (row, col) else None) line)
      grid)

let get_pos grid (row, col) = grid.(row).(col)
let set_pos grid (row, col) value = grid.(row).(col) <- value
let ( .@() ) = get_pos
let ( .@()<- ) = set_pos

let of_lines of_char lines =
  Array.of_seq (Seq.map (fun line -> Array.of_seq (Seq.map of_char (String.to_seq line))) lines)

let pp ?(highlight = []) ?(sgr = "\x1b[42m") f fmt grid =
  Array.iteri
    (fun row line ->
      Array.iteri
        (fun col value ->
          if List.mem (row, col) highlight then (
            Format.(
              pp_print_string fmt sgr;
              f fmt value;
              pp_print_string fmt "\x1b[0m"))
          else f fmt value)
        line;
      Format.pp_print_newline fmt ())
    grid
