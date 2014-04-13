open Riak
open Lazy
open Lwt
open Bin_prot
open Bin_prot_utils

type ('k, 'v) cache = {connection:riak_connection; bucket:string; key_serializer:'k string_serializer;value_serializer:('v string_serializer)}

lwt connection = riak_connect_with_defaults "localhost" 8087

let create key_serializer value_serializer bucket = {connection;bucket;key_serializer;value_serializer}

let put cache key value = 
  let serialized_key = cache.key_serializer.to_string key in
  let serialized_value = cache.value_serializer.to_string value in
  lwt result = riak_put cache.connection cache.bucket (Some serialized_key) serialized_value [] in Lwt.return ()

let get cache key =
  let serialized_key = cache.key_serializer.to_string key in
  lwt result = riak_get cache.connection cache.bucket serialized_key [] in 
Lwt.return (match result with Some r -> 
  (match r.obj_value with Some s -> Some (cache.value_serializer.from_string s) | None -> None)
                            | None -> None)

let exists cache key = lwt result = (get cache key) in Lwt.return(result != None)
