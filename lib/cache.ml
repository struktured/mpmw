open Riak 
open Lwt
open Bin_prot.Std
open Bin_prot_utils
open Lwt_zmq
open Core.Std
open Deriving_Show
open Bin_prot_utils

type ('k, 'v) entry = {key:'k;value:'v} with bin_io

type raw_entry = (string, string) entry with bin_io
type 'v entry_update = {old_value:'v;new_value:'v} with bin_io

type ('k,'v) cache_operation = Insert of ('k, 'v) entry | Delete of ('k, 'v) entry | Update of 'k * 'v entry_update with bin_io

type raw_cache_operation = (string,string) cache_operation with bin_io
  
type ('k,'v) listener = ('k,'v) cache_operation -> unit 

let noop_listener (o:('k, 'v) cache_operation) = ()
let create_entry ~(key:'k) ~(value:'v) = {key;value}

type ('k, 'v) t = {connection:riak_connection; bucket:string; 
  publisher:raw_cache_operation Publisher.t;key_serializer:'k string_serializer;
  value_serializer:('v string_serializer);cache_operation_serializer:raw_cache_operation string_serializer;subscriber:raw_cache_operation Subscriber.t}

(* Provisional: need to manage this explicitly, possibly within remote context? *)
lwt connection = riak_connect_with_defaults "localhost" 8087

let create_publisher address serializer = Publisher.create (Remote_context.get()) address serializer 

(* Provisional: need to make a resource file for this or something of that nature *)
(*let client_socket_address = Address.create ~transport:Transport.EPGM ~endpoint:"eth0;239.192.1.1:5555" *)

let subscriber_socket_address = Address.create ~transport:Transport.EPGM ~endpoint:"eth0;239.192.1.1;5556"

let publisher_socket_address = subscriber_socket_address


let deserialize_raw_cache_operation (op:raw_cache_operation) (key_serializer: 'k string_serializer) (value_serializer:'v string_serializer) =
 let key_from_string = Bin_prot_utils.make_from_string key_serializer in 
 let value_from_string = Bin_prot_utils.make_from_string value_serializer in
 match op with
    Insert {key;value} -> (Insert {key=key_from_string key; value=value_from_string value})
  | Update (key, {old_value;new_value}) -> (Update (key_from_string key, {old_value=value_from_string old_value;
      new_value=value_from_string new_value}))
  | Delete {key;value} -> (Delete {key=key_from_string key;value=value_from_string value})

  
let setup_subscriber (listener:('k, 'v) cache_operation -> unit) bucket (serializer:raw_cache_operation string_serializer) 
 key_serializer value_serializer = 
  let initial_state = () in
  let f operation state = listener (deserialize_raw_cache_operation operation key_serializer value_serializer) in
  let subscriber = Subscriber.create ~context:(Remote_context.get()) ~address:subscriber_socket_address ~serializer in
  let _ = Subscriber.subscribe subscriber ~topic:bucket ~f ~initial_state in subscriber

let create ~(key_serializer:'k string_serializer) ~(value_serializer:'v string_serializer) ~(bucket:string) ?(listener=noop_listener) () =
  let cache_operation_serializer = Bin_prot_utils.create bin_read_raw_cache_operation bin_write_raw_cache_operation in
  let subscriber = setup_subscriber listener bucket cache_operation_serializer key_serializer value_serializer in
  let cache:('k, 'v) t = {connection;bucket;key_serializer;value_serializer;cache_operation_serializer;
  publisher=create_publisher publisher_socket_address cache_operation_serializer;subscriber} in cache

let notify_listener cache (operation:raw_cache_operation) =
  Publisher.publish cache.publisher ~topic:cache.bucket ~data:operation

let put (cache:('k,'v) t) (key:'k) (value:'v) = 
  let serialized_key = make_to_string cache.key_serializer key in
  let serialized_value = make_to_string cache.value_serializer value in
  lwt result = riak_put cache.connection cache.bucket (Some serialized_key) serialized_value [] in
  let raw_cache_operation = match result with 
      None -> Insert (create_entry ~key:serialized_key ~value:serialized_value)
    | Some result' -> match result'.obj_value with 
        None -> Insert (create_entry ~key:serialized_key ~value:serialized_value) 
      | Some prev_serialized_value -> Update (serialized_key, {old_value=prev_serialized_value;new_value=serialized_value}) in  
  notify_listener cache raw_cache_operation

let get (cache:('k, 'v) t) key =
  let serialized_key = make_to_string cache.key_serializer key in
  lwt result = riak_get cache.connection cache.bucket serialized_key [] in 
  Lwt.return (match result with Some r -> 
  (match r.obj_value with Some s -> Some (make_from_string cache.value_serializer s) | None -> None)
                            | None -> None)

let exists cache key = lwt result = (get cache key) in Lwt.return(not (result = None))
