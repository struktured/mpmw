(* This module creates and destroys remote contexts. These are used to encapsulate all the initialized state of the 
 * underlying middlewares used for caching and messaging. It requires a configuration, can be created programmatically 
 * or linked in via ocaml_plugin. See examples/custom_config.ml for details *)

(* The type of a remote context. This is a container of all configuration and initial state of the middleware system. 
 * In practice it holds a reference to a zero mq context and the configuration it was initialized by. *)
type t 

(* Creates a remote context given a configuration. *)
val create : Config.t -> t

(* Destroys a remote context. It can no longer be used after this invocation. Further usage of this context is undefined *)
val destroy : t -> unit

(* Gets the configuaration associated with a context *)
val config t -> Config.t
