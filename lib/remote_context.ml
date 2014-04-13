open ZMQ

type t = context

let ref_context = ref None

let rec get() = match (!ref_context) with 
    Some c -> c  
  | None -> (ref_context := Some (ZMQ.init());get())


  (* add assert on ref_context = context or elim this arg *)
let destroy context = ZMQ.term context; ref_context := None
