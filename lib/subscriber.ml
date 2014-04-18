open Bin_prot_utils
open Lwt_zmq
open Core.Std

type 'a t = {context:Remote_context.t;socket:([`Sub]) Socket.t; 
             serializer:('a string_serializer)} 

let create ~context ~address ~serializer =
  print_endline ("about to create subscriber for " ^ address);
  let socket = ZMQ.Socket.create context ZMQ.Socket.sub in
  let lwt_socket = Socket.of_socket socket in 
  print_endline ("binding to address: " ^ address);
  ZMQ.Socket.connect socket address;
  print_endline ("bound address" ^ address);
  print_endline ("subscribe address " ^ address);
  {context;socket=lwt_socket;serializer}

let topic_delim = ':'

exception Missing_topic of string
let subscribe subscriber ~topic ~f ~initial_state =
  ZMQ.Socket.subscribe (Socket.to_socket subscriber.socket) topic;
  let rec loop state = 
    lwt serialized_data_with_topic = Socket.recv subscriber.socket in
  match String.split ~on:topic_delim serialized_data_with_topic
  with
    [topic';serialized_data] -> (* ssert_lwt (topic' = topic)); *)
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

