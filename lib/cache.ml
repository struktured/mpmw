open Riak
open Lwt
open Bin_prot.Std
open Bin_prot_utils
open Lwt_zmq
open Core.Std
open Deriving_Show
open Publisher

type ('k, 'v) entry = {key:'k;value:'v} with bin_io, show

type raw_entry = (string, string) entry
type 'v entry_update = {old_value:'v;new_value:'v} with bin_io, show

type ('k,'v) cache_operation = Insert of ('k, 'v) entry | Delete of ('k, 'v) entry | Update of 'k * 'v entry_update with bin_io, show

type raw_cache_operation = (string,string) cache_operation
  
type ('k,'v) listener = ('k,'v) cache_operation -> unit 

let create_entry ~(key:'k) ~(value:'v) = {key;value}

type ('k, 'v) cache = {connection:riak_connection; bucket:string; publisher:('k, 'v) cache_operation Publisher.t;key_serializer:'k string_serializer;
  value_serializer:('v string_serializer);cache_operation_serializer:('k, 'v) cache_operation string_serializer;mutable listeners:('k, 'v) listener list}
lwt connection = riak_connect_with_defaults "localhost" 8087

let channel_of_bucket bucket = bucket
let create_publisher bucket serializer = Publisher.create (Remote_context.get()) (channel_of_bucket bucket) serializer 

let setup_subscriber cache = 
  let initial_state = () in
  let f operation state = List.iter cache.listeners (fun l -> l operation) in
  Subscriber.subscribe ~topic:cache.bucket ~f ~initial_state 

let create (key_serializer:'k string_serializer) (value_serializer:'v string_serializer) bucket =
  let writer = bin_write_cache_operation key_serializer.write_fun value_serializer.write_fun in
  let reader = bin_read_cache_operation key_serializer.read_fun value_serializer.read_fun in
  let cache_operation_serializer = Bin_prot_utils.create reader writer in 
  let cache = {connection;bucket;key_serializer;value_serializer;cache_operation_serializer;
  publisher=create_publisher bucket cache_operation_serializer;listeners=[]} in
  let subscriber = setup_subscriber cache in cache

let notify_listeners cache operation =
  Publisher.publish cache.publisher ~topic:cache.bucket ~data:operation

let put (cache:('k,'v) cache) (key:'k) (value:'v) = 
  let serialized_key = make_to_string cache.key_serializer key in
  let serialized_value = make_to_string cache.value_serializer value in
  lwt result = riak_put cache.connection cache.bucket (Some serialized_key) serialized_value [] in
  let raw_cache_operation = match result with 
      None -> Insert (create_entry ~key:serialized_key ~value:serialized_value)
    | Some result' -> match result'.obj_value with 
        None -> Insert (create_entry ~key:serialized_key ~value:serialized_value) 
      | Some prev_serialized_value -> Update (serialized_key, {old_value=prev_serialized_value;new_value=serialized_value}) in  
  Lwt.return (notify_listeners cache raw_cache_operation)

let get (cache:('k, 'v) cache) key =
  let serialized_key = make_to_string cache.key_serializer key in
  lwt result = riak_get cache.connection cache.bucket serialized_key [] in 
  Lwt.return (match result with Some r -> 
  (match r.obj_value with Some s -> Some (make_from_string cache.value_serializer s) | None -> None)
                            | None -> None)

let exists cache key = lwt result = (get cache key) in Lwt.return(not (result = None))

let add_map_listener cache ~listener = 
  Lwt.return (cache.listeners <- cache.listeners@[listener])

let remove_map_listener cache ~listener =
  Lwt.return (cache.listeners <- List.filter cache.listeners ~f:((=) listener))


