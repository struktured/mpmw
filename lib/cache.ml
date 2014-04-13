open Riak
open Lwt
open Bin_prot
open Bin_prot_utils
open Lwt_zmq
open Core.Std
open Deriving_Show

type ('k, 'v) entry = {key:'k;value:'v} with bin_io, show

type ('k, 'v) cache = {connection:riak_connection; bucket:string; publisher:('k, 'v) entry Publisher.t;key_serializer:'k string_serializer;
  value_serializer:('v string_serializer);mutable listeners:('k -> 'v -> unit) list}
lwt connection = riak_connect_with_defaults "localhost" 8087

let create key_serializer value_serializer bucket = {connection;bucket;key_serializer;value_serializer;publisher=create_publisher listeners=[]}

let channel_of_bucket bucket = bucket

let notify_listeners cache ~key ~value = 
(*  List.iter cache.listeners (fun listener -> listener key value); *)

  let publisher = Publisher.create (Remote_context.get()) (channel_of_bucket cache.bucket) cache.key_serializer in
  publisher

let put cache ~key ~value = 
  let serialized_key = cache.key_serializer.to_string key in
  let serialized_value = cache.value_serializer.to_string value in
  lwt result = riak_put cache.connection cache.bucket (Some serialized_key) serialized_value [] in 
  Lwt.return (notify_listeners cache key value)

let get cache key =
  let serialized_key = cache.key_serializer.to_string key in
  lwt result = riak_get cache.connection cache.bucket serialized_key [] in 
  Lwt.return (match result with Some r -> 
  (match r.obj_value with Some s -> Some (cache.value_serializer.from_string s) | None -> None)
                            | None -> None)

let exists cache key = lwt result = (get cache key) in Lwt.return(not (result = None))

let add_map_listener cache ~listener = 
  Lwt.return (cache.listeners <- cache.listeners@[listener])

let remove_map_listener cache ~listener =
  Lwt.return (cache.listeners <- List.filter cache.listeners ~f:((=) listener))


