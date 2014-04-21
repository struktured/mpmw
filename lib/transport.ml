open Bin_prot
open Deriving_Show
open Core.Std

type t = PGM | EPGM | UDP | TCP with bin_io, show

let string_of_transport transport = String.lowercase (Show_t.show transport)
