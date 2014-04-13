open Bin_prot.Std
open Bin_prot.Read
open Bin_prot.Write
open Bin_prot.Common
open Core.Std

type 'a to_string = 'a -> string
type 'a from_string = string -> 'a

type 'a string_serializer = {to_string:'a to_string; from_string: 'a from_string}

let rec fill chars buf pos = match chars with [] -> buf | hd::rest -> let i = bin_write_char buf pos hd in (fill rest buf i);;
let serialize_as_chars buf l' = let rec iter z = let c = (bin_read_char buf ~pos_ref:z) in [(c)]@(if (!z) < l' then (iter z) else []) in iter (ref 0);;

let max_buf_size = 131072
let start_pos = 0

let refPos () : pos_ref = ref start_pos

let create_max_size_buf ()  = create_buf max_buf_size

let to_chars to_buf value = 
  let buf = create_max_size_buf() in
  let written = to_buf buf ~pos:start_pos value in serialize_as_chars buf written

let to_string to_buf value = String.concat (List.map (to_chars to_buf value) Char.to_string)

let string_to_chars s = String.fold s ~init:[] ~f:(fun l c -> l@[c])

let from_string from_buf value = 
  let pos_ref = refPos () in
  let buf = fill (string_to_chars value) (create_max_size_buf ()) start_pos in from_buf buf ~pos_ref
