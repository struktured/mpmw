open Bin_prot_utils

type ('a, 'b) t

val create : context:Remote_context.t -> address:Address.t -> request_serializer:('a string_serializer) 
  -> response_serializer:('b string_serializer) -> ('a, 'b) t

val invoke : ('a, 'b) t -> 'a -> 'b

val destroy : ('a, 'b) t -> unit



