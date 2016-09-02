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
      
  val empty : t
  val receive_offer_request : t -> Otr.Otrdata.offer_request -> t * bool
  val receive_get_request : t -> string -> t * offer option
  val receive_response : t -> string -> string -> t * get_response_result
  val make_offer : t -> request_id:string -> file_path:string -> hex_sha1:string -> file_length:int64 -> t * string
end
