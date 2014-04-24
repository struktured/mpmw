open Bin_prot_utils
open Lwt_zmq
open Core.Std

type 'a t = {context:Remote_context.t;socket:([`Sub]) Socket.t; 
             serializer:('a string_serializer)} 

type ('a, 'b) subscription = {subscriber:'a t;thread:'b Lwt.t Lwt.t;topic:string}

exception Invalid_transport of Transport.t

let create ~context ~address ~serializer = 
  let address_as_string = Address.string_of_address address in
  Lwt.ignore_result (Lwt_io.printl ("about to create subscriber for " ^ address_as_string));
  let socket = ZMQ.Socket.create context ZMQ.Socket.sub in
  let lwt_socket = Socket.of_socket socket in 
  Lwt.ignore_result (Lwt_io.printl ("binding to address: " ^ address_as_string));
  ZMQ.Socket.connect socket address_as_string;
  Lwt.ignore_result (Lwt_io.printl ("bound address: " ^ address_as_string));
  {context;socket=lwt_socket;serializer}

let topic_delim = ':'

exception Missing_topic of string

let subscribe subscriber ~topic ~(f:'a -> 'b -> 'b) ~(initial_state:'b) : ('a, 'b) subscription =
  ZMQ.Socket.subscribe (Socket.to_socket subscriber.socket) topic;
  let rec loop state = 
    Lwt.ignore_result (Lwt_io.printl ("[subscribe loop] start")); 
    lwt serialized_data_with_topic = Socket.recv subscriber.socket in
    Lwt.ignore_result (Lwt_io.printl ("[subscribe loop] serialized_data: " ^ serialized_data_with_topic));
    match String.split ~on:topic_delim serialized_data_with_topic with
      [topic';serialized_data] -> assert (topic' = topic);
      Lwt.ignore_result (Lwt_io.printl ("[subscribe loop] topic: " ^ topic)); 
      let deserialized_data = Bin_prot_utils.make_from_string subscriber.serializer serialized_data in
      let state' = f deserialized_data state in loop state'
    |  _-> raise (Missing_topic serialized_data_with_topic)
in
{subscriber;thread=Lwt_preemptive.detach loop initial_state;topic}

let unsubscribe subscription = 
  ZMQ.Socket.unsubscribe (Socket.to_socket subscription.subscriber.socket) subscription.topic;
  Lwt.cancel subscription.thread

let destroy subscriber = 
  let socket = Socket.to_socket subscriber.socket in
  ZMQ.Socket.close (Socket.to_socket subscriber.socket)

