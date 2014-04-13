open Bin_prot_utils

type ('a, 'b, 'c) t

val create : context:Remote_context.t -> channel:string -> request_serializer:('a string_serializer) -> 
  response_serializer:('b string_serializer) -> initial_state:'c -> callback:('a -> 'c -> 'b * 'c) -> ('a, 'b, 'c) t

val destroy : ('a, 'b, 'c) t -> unit

(* blocks forever? *)
val respond : ('a, 'b, 'c) t -> unit  

