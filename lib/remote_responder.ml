open Bin_prot_utils
open ZMQ
open ZMQ.Socket

type ('a, 'b, 'c) t  = {context:Remote_context.t;responder:([`Rep]) Socket.t; 
  request_serializer:('a string_serializer); response_serializer:('b string_serializer);
  initial_state:'c; callback:('a -> 'c -> 'b * 'c)}

let create ~context ~channel ~request_serializer ~response_serializer ~initial_state ~callback =
  print_endline ("about to create responder for " ^ channel);
  let responder = Socket.create (Remote_context.get()) rep in
  print_endline ("binding to channel: " ^ channel);
  bind responder channel;
  print_endline ("bound channel" ^ channel);
  {context;responder;request_serializer;response_serializer;initial_state;callback}

let respond responder =
  let state_ref = ref responder.initial_state in 
  while true do
    let request_as_string = recv responder.responder in
    print_endline ("request: " ^ request_as_string);
    let request = responder.request_serializer.from_string request_as_string in
    let response, state = responder.callback request (!state_ref) in 
    let response_as_string = responder.response_serializer.to_string response in
    print_endline ("response: " ^ response_as_string);
    send responder.responder response_as_string;
    state_ref := state
  done

let destroy responder = close responder.responder

