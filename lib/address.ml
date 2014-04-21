type t = {transport:Transport.t;endpoint:Endpoint.t}

let create ~transport ~endpoint = {transport; endpoint=Endpoint.create endpoint}

let delim = "://"

let string_of_address address = (Transport.string_of_transport address.transport) ^ delim ^ (Endpoint.string_of_endpoint address.endpoint)

let transport_of address = address.transport
let endpoint_of address = address.endpoint
