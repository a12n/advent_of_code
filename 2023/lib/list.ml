include Stdlib.List

let rec diffs sub = function
  | [ x0; x1 ] -> [ sub x1 x0 ]
  | x0 :: (x1 :: _ as xs) -> sub x1 x0 :: diffs sub xs
  | [ _ ] | [] -> invalid_arg __FUNCTION__

let rec last = function [ x0 ] -> x0 | _ :: xs -> last xs | [] -> invalid_arg __FUNCTION__

let rec pairs = function
  | [] -> []
  | x0 :: x1 :: xs -> (x0, x1) :: pairs xs
  | _ -> invalid_arg __FUNCTION__

let reduce f = function x0 :: xs -> fold_left f x0 xs | [] -> invalid_arg __FUNCTION__
let rec unpair = function [] -> [] | (x0, x1) :: xs -> x0 :: x1 :: unpair xs
