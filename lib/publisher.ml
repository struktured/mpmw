open Bin_prot_utils
open Lwt_zmq
open Core.Std

type 'a t = {context:Remote_context.t;publisher:([`Pub]) Socket.t; 
             serializer:('a string_serializer)} 

let create ~context ~address ~serializer =
  let socket = ZMQ.Socket.create context ZMQ.Socket.pub in
  let publisher = Socket.of_socket socket in 
  let (_:unit) = ZMQ.Socket.bind socket (Address.string_of_address address) in 
  {context;publisher;serializer}

let topic_delim = ':' 

exception Invalid_topic_name of string

let validate topic = if String.contains topic topic_delim || String.is_empty topic then raise (Invalid_topic_name topic) else () 

let publish_serialized publisher ~topic ~serialized_data =
  Lwt.ignore_result (Lwt_io.printl ("data: " ^ serialized_data));
  Socket.send publisher.publisher (topic ^ (Char.to_string topic_delim) ^ serialized_data)

let publish publisher ~topic ~data =
  let serialized_data = Bin_prot_utils.make_to_string publisher.serializer data in
  publish_serialized publisher ~topic ~serialized_data

let destroy publisher = ZMQ.Socket.close (Socket.to_socket publisher.publisher)

