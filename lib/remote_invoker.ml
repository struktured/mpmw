open Bin_prot_utils
open ZMQ
open ZMQ.Socket

type ('a, 'b) t  = {context:Remote_context.t;requester:([`Req]) Socket.t; 
  request_serializer:('a string_serializer); response_serializer:('b string_serializer)}

let get_context () = Remote_context.get()

let create ~context ~channel ~request_serializer 
  ~response_serializer = 
    let requester = Socket.create context req in
    connect requester channel;
    let invoker = {context;requester;request_serializer;response_serializer} in
    print_endline ("created remote invoker");invoker

let invoke invoker req = 
  send invoker.requester (make_to_string invoker.request_serializer req);
  let response = recv invoker.requester in (make_from_string invoker.response_serializer response)

let destroy invoker = close invoker.requester





