open Bin_prot
open Deriving_Show
open Bin_prot.Std

type t = string with bin_io, show

let create value = value

let string_of_endpoint e = e

