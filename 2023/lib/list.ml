include Stdlib.List

let rec diffs sub = function
  | [ x0; x1 ] -> [ sub x1 x0 ]
  | x0 :: (x1 :: _ as xs) -> sub x1 x0 :: diffs sub xs
  | [ _ ] | [] -> invalid_arg __FUNCTION__

let rec make n elt =
  match n with 0 -> [] | n when n > 0 -> elt :: make (n - 1) elt | _ -> invalid_arg __FUNCTION__

let intersperse sep =
  let rec prepend = function [] -> [] | x0 :: xs -> sep :: x0 :: prepend xs in
  function [] -> [] | x0 :: xs -> x0 :: prepend xs

let rec last = function [ x0 ] -> x0 | _ :: xs -> last xs | [] -> invalid_arg __FUNCTION__
let rec combine_tl = function [] -> [] | [ _ ] -> [] | x0 :: xs -> (x0, xs) :: combine_tl xs

let rec take_pairs = function
  | [] -> []
  | x0 :: x1 :: xs -> (x0, x1) :: take_pairs xs
  | _ -> invalid_arg __FUNCTION__

let reduce f = function x0 :: xs -> fold_left f x0 xs | [] -> invalid_arg __FUNCTION__
let rec untake_pairs = function [] -> [] | (x0, x1) :: xs -> x0 :: x1 :: untake_pairs xs
