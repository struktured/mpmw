open Bin_prot_utils
open Lwt_zmq
open Core.Std

type 'a t = {context:Remote_context.t;socket:([`Sub]) Socket.t; 
             serializer:('a string_serializer)} 

exception Invalid_transport of Transport.t

let create ~context ~address ~serializer = 
  let transport = Address.transport_of address in  
  if (not (transport = Transport.PGM) && (not (transport = Transport.EPGM))) then raise (Invalid_transport transport);
  let address_as_string = Address.string_of_address address in
  print_endline ("about to create subscriber for " ^ address_as_string);
  let socket = ZMQ.Socket.create context ZMQ.Socket.sub in
  let lwt_socket = Socket.of_socket socket in 
  print_endline ("binding to address: " ^ address_as_string);
  ZMQ.Socket.connect socket address_as_string;
  print_endline ("bound address" ^ address_as_string);
  print_endline ("subscribe address " ^ address_as_string);
  {context;socket=lwt_socket;serializer}

let topic_delim = ':'

exception Missing_topic of string

let subscribe subscriber ~topic ~f ~initial_state =
  ZMQ.Socket.subscribe (Socket.to_socket subscriber.socket) topic;
  let rec loop state = 
    lwt serialized_data_with_topic = Socket.recv subscriber.socket in
  match String.split ~on:topic_delim serialized_data_with_topic
  with
    [topic';serialized_data] -> assert (topic' = topic);
    let deserialized_data = Bin_prot_utils.make_from_string subscriber.serializer serialized_data in
    let state' = f deserialized_data state in loop state'
  | _-> raise (Missing_topic serialized_data_with_topic)
in
Lwt_preemptive.detach loop initial_state

let unsubscribe subscriber ~topic =
  ZMQ.Socket.unsubscribe (Socket.to_socket subscriber.socket) topic

let destroy subscriber = 
  let socket = Socket.to_socket subscriber.socket in
  ZMQ.Socket.close (Socket.to_socket subscriber.socket)

