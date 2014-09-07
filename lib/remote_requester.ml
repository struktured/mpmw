open Bin_prot_utils
open ZMQ
open ZMQ.Socket
open Core.Std

type ('a, 'b) t  = {context:Remote_context.t;requester:([`Req]) Socket.t; 
  request_serializer:('a string_serializer); response_serializer:('b string_serializer)}

let create ~context ~address ~request_serializer 
  ~response_serializer = 
    let requester = Socket.create context req in
    let address_as_string = Address.string_of_address address in
    connect requester address_as_string;
    let requester = {context;requester;request_serializer;response_serializer} in
    print_endline ("created remote requester");requester

let request requester req = 
  send requester.requester (make_to_string requester.request_serializer req);
  let response = recv requester.requester in (make_from_string requester.response_serializer response)

let destroy requester = close requester.requester





