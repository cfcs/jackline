module State : sig
  type t
  type offer =
    { remote_filename : string
    ; local_filename : string
    ; sha1 : string
    ; total_size : int64
    }
  type parsed_get_response =
    { filename : string
    ; offset : int64
    ; content : string
    }
  type get_response_result =
    | Get_response of parsed_get_response
    | Invalid_content
    | Unknown_request_id
      
  val create : unit -> t
  val receive_offer_request : t -> Otr.Otrdata.offer_request -> t * bool
  val receive_get_request : t -> string -> offer option
  val receive_response : t -> string -> string -> t * get_response_result
end
