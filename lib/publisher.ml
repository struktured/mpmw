open Bin_prot_utils
open Lwt_zmq

type 'a t = {context:Remote_context.t;publisher:([`Pub]) Socket.t; 
  serializer:('a string_serializer)} 
 
let create ~context ~channel ~serializer =
  print_endline ("about to create publisher for " ^ channel);
  let socket = ZMQ.Socket.create (Remote_context.get()) ZMQ.Socket.pub in
  let publisher = Socket.of_socket socket in 
  print_endline ("binding to channel: " ^ channel);
  ZMQ.Socket.bind socket channel;
  print_endline ("bound channel" ^ channel);
  {context;publisher;serializer}

let publish publisher ~data =
    let serialized_data = Bin_prot_utils.make_to_string publisher.serializer data in
    print_endline ("data: " ^ serialized_data);
    Socket.send publisher.publisher serialized_data

let destroy publisher = ZMQ.Socket.close (Socket.to_socket publisher.publisher)

