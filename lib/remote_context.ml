
type t = {zmq_contex:ZMQ.context;config:Config.t}

let create config = {zmq_cont:xt=ZMQ.init();config}

let destroy context = ZMQ.term context
