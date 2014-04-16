open Bin_prot_utils
open Lwt_zmq

type 'a t = {context:Remote_context.t;socket:([`Sub]) Socket.t; 
             serializer:('a string_serializer)} 

let create ~context ~channel ~serializer =
  print_endline ("about to create subscriber for " ^ channel);
  let socket = ZMQ.Socket.create context ZMQ.Socket.sub in
  let lwt_socket = Socket.of_socket socket in 
  print_endline ("binding to channel: " ^ channel);
  ZMQ.Socket.connect socket channel;
  print_endline ("bound channel" ^ channel);
  print_endline ("subscribe channel" ^ channel);
  {context;socket=lwt_socket;serializer}

let subscribe subscriber ~topic ~f ~initial_state =
  ZMQ.Socket.subscribe (Socket.to_socket subscriber.socket) topic;
  Lwt.return (
    let rec loop state = 
    lwt serialized_data = Socket.recv subscriber.socket in
    let deserialied_data = Bin_prot_utils.make_from_string subscriber.serializer serialized_data in
    let state' = f state in
    loop state in
    loop initial_state
  )

let destroy subscriber = 
  let socket = Socket.to_socket subscriber.socket in
  ZMQ.Socket.close (Socket.to_socket subscriber.socket)

