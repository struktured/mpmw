open Bin_prot_utils
open ZMQ
open ZMQ.Socket

type ('a, 'b, 'c) t  = {context:Remote_context.t;responder:([`Rep]) Socket.t; 
  request_serializer:('a string_serializer); response_serializer:('b string_serializer);
  initial_state:'c; callback:('a -> 'c -> 'b * 'c)}

let create ~context ~address ~request_serializer ~response_serializer ~initial_state ~callback =
  let responder = Socket.create (Remote_context.get()) rep in
  let address_as_string = Address.string_of_address address in 
  print_endline ("binding to channel: " ^ address_as_string);
  bind responder address_as_string;
  print_endline ("bound channel" ^ address_as_string);
  {context;responder;request_serializer;response_serializer;initial_state;callback}

let respond responder =
  let state_ref = ref responder.initial_state in 
  while true do
    let request_as_string = recv responder.responder in
    print_endline ("request: " ^ request_as_string);
    let request = make_from_string responder.request_serializer request_as_string in
    let response, state = responder.callback request (!state_ref) in 
    let response_as_string = make_to_string responder.response_serializer response in
    print_endline ("response: " ^ response_as_string);
    send responder.responder response_as_string;
    state_ref := state
  done

let destroy responder = close responder.responder

