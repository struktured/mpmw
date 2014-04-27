open Bin_prot_utils

(* The type of a responder. 'a is the request type, 'b is the response type, and 'c is the state type *)
type ('a, 'b, 'c) t

(* Creates a new responder given a context, address, serializers, initial state and callback. Once the responder is started
 * it will invoke the callback with the initial state and initial request, and will loop continually but with each successive
 * state replacing the previous. *)
val create : context:Remote_context.t -> address:Address.t -> request_serializer:('a string_serializer) -> 
  response_serializer:('b string_serializer) -> initial_state:'c -> callback:('a -> 'c -> 'b * 'c) -> ('a, 'b, 'c) t

val destroy : ('a, 'b, 'c) t -> unit

(* blocks forever? *)
val respond : ('a, 'b, 'c) t -> unit  
