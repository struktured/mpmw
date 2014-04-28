open Bin_prot_utils
open ZMQ
open ZMQ.Socket

type ('a, 'b, 'c) t  = {context:Remote_context.t;responder:([`Rep]) Socket.t; 
  request_serializer:('a string_serializer); response_serializer:('b string_serializer);
  callback:('a -> 'c -> 'b * 'c); thread:unit Lwt.t option}

let respond responder initial_state =
  let state_ref = ref initial_state in 
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

let create ~context ~address ~request_serializer ~response_serializer ~initial_state ~callback =
  let responder = Socket.create (Remote_context.get()) rep in
  let address_as_string = Address.string_of_address address in 
  print_endline ("binding to address: " ^ address_as_string);
  bind responder address_as_string;
  print_endline ("bound address: " ^ address_as_string);
  let responder = {context;responder;request_serializer;response_serializer;callback;thread=None} in
  let thread' = Some (Lwt_preemptive.detach (respond responder) initial_state) in
  let responder' = {responder with thread=thread'} in responder'

let destroy responder = (match responder.thread with Some t -> Lwt.cancel t | None -> ()) ; close responder.responder

