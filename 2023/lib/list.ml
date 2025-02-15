include Stdlib.List

let rec diffs sub = function
  | [ x0; x1 ] -> [ sub x1 x0 ]
  | x0 :: (x1 :: _ as xs) -> sub x1 x0 :: diffs sub xs
  | [ _ ] | [] -> invalid_arg __FUNCTION__

let fold_lefti f =
  let rec loop i acc = function [] -> acc | x0 :: xs -> loop (i + 1) (f acc i x0) xs in
  loop 0

let rec make n elt =
  match n with 0 -> [] | n when n > 0 -> elt :: make (n - 1) elt | _ -> invalid_arg __FUNCTION__

let intersperse sep =
  let rec prepend = function [] -> [] | x0 :: xs -> sep :: x0 :: prepend xs in
  function [] -> [] | x0 :: xs -> x0 :: prepend xs

let rec last = function [ x0 ] -> x0 | _ :: xs -> last xs | [] -> invalid_arg __FUNCTION__
let rec combine_tl = function [] -> [] | [ _ ] -> [] | x0 :: xs -> (x0, xs) :: combine_tl xs

let rec remove_if pred = function
  | [] -> []
  | x0 :: xs when pred x0 -> xs
  | x0 :: xs -> x0 :: remove_if pred xs

let replace_assoc k v assoc =
  match
    fold_left_map
      (fun ok (ki, vi) -> if ki = k then (true, (ki, v)) else (ok, (ki, vi)))
      false assoc
  with
  | true, assoc -> assoc
  | false, assoc -> (k, v) :: assoc

let rec take_pairs = function
  | [] -> []
  | x0 :: x1 :: xs -> (x0, x1) :: take_pairs xs
  | _ -> invalid_arg __FUNCTION__

let reduce_opt f = function x0 :: xs -> Some (fold_left f x0 xs) | [] -> None
let reduce f xs = Option.get (reduce_opt f xs)
let rec untake_pairs = function [] -> [] | (x0, x1) :: xs -> x0 :: x1 :: untake_pairs xs
