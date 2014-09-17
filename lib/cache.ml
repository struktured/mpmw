open Riak 
open Lwt
open Bin_prot.Std
open Bin_prot_utils
open Lwt_zmq
open Core.Std
open Deriving_Show
open Bin_prot_utils


module type S = 
sig
type key
type value
type entry 
type entry_update
type t
type cache_operation = Insert of entry | Delete of entry | Update of entry_update with bin_io
type listener = cache_operation -> unit 
end

module Make (Key:Binable) (Value:Binable) : S = 
struct
type key = Key.t
type value = Value.t
type entry = {key:key;value:value} with bin_io

type entry_update = {old_value:value;new_value:value} with bin_io
type cache_operation = Insert of entry | Delete of entry | Update of entry_update with bin_io
type listener = cache_operation -> unit 



type t = {connection:riak_connection; bucket:string; 
            ;key_serializer:key string_serializer;
            value_serializer:(value string_serializer);cache_operation_serializer:cache_operation string_serializer}

(*let setup_subscriber listener bucket serializer
      key_serializer value_serializer = 
  let initial_state = () in
  let f operation state = listener operation in
  let subscriber = Subscriber.create ~context:(Remote_context.get()) ~address:subscriber_socket_address ~serializer in
  let _ = Subscriber.subscribe subscriber ~topic:bucket ~f ~initial_state in subscriber
end *)


  let create_entry ~(key:key) ~(value:value) = {key;value} 

  let _create ~(key_serializer) ~(value_serializer:) ~(bucket:string) listener () =
    let cache_operation_serializer = Bin_prot_utils.create bin_read_cache_operation bin_write_cache_operation in
    {connection;bucket;key_serializer;value_serializer;cache_operation_serializer}
end

  let create bucket =
    let key_serializer = Bin_prot_utils.create Key.bin_read_t Key.bin_write_t in
    let value_serializer = Bin_prot_utils.create Value.bin_read_t Value.bin_write_t in
    _create ~key_serializer ~value_serializer ~bucket ()


  let notify_listener cache operation =
    Publisher.publish cache.publisher ~topic:cache.bucket ~data:operation


  let put (cache:t) key value = 
    let serialized_key = make_to_string cache.key_serializer key in
    let serialized_value = make_to_string cache.value_serializer value in
    lwt result = riak_put cache.connection cache.bucket (Some serialized_key) serialized_value [] in
    Lwt.return(())
 (* let raw_cache_operation = match result with 
      None -> Insert (create_entry ~key:serialized_key ~value:serialized_value)
    | Some result' -> match result'.obj_value with 
        None -> Insert (create_entry ~key:serialized_key ~value:serialized_value) 
      | Some prev_serialized_value -> Update (serialized_key, {old_value=prev_serialized_value;new_value=serialized_value}) in  
  notify_listener cache raw_cache_operation
*)
  let get (cache:t) key =
    let serialized_key = make_to_string cache.key_serializer key in
    lwt result = riak_get cache.connection cache.bucket serialized_key [] in 
  Lwt.return (match result with Some r -> 
    (match r.obj_value with Some s -> Some (make_from_string cache.value_serializer s) | None -> None)
                              | None -> None)

  let exists (cache:t) key = lwt result = (get cache key) in Lwt.return(not (result = None))

  
(* Provisional: need to manage this explicitly, possibly within remote context? *)
lwt connection = riak_connect_with_defaults "localhost" 8087


let create_publisher address serializer = Publisher.create (Remote_context.get()) address serializer 

(* Provisional: need to make a resource file for this or something of that nature *)
(*let client_socket_address = Address.create ~transport:Transport.EPGM ~endpoint:"eth0;239.192.1.1:5555" *)

let subscriber_socket_address = Address.create ~transport:Transport.EPGM ~endpoint:"eth0;239.192.1.1;5556"

let publisher_socket_address = subscriber_socket_address

end


