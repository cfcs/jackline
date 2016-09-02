module State = struct
  let chunk_size = 60_000_L (* request must be < 65535, TODO algorithm *)
  let pipeline_size = 1     (* number of concurrent requests *)

  type sent_request =
    { remote_filename : string
    ; request_id : string
    ; range_start : int64
    ; range_end : int64
    }

  type incomplete_file =
    { local_filename : string
    ; remote_filename : string
    ; total_size : int64
    ; missing_ranges : (int64 * int64) list
    ; sha1_hash : string
    }
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

  type t = { sent_offers : offer list
           ; received_offers : offer list
           ; incomplete_files : incomplete_file list
           ; sent_requests : sent_request list
           }
             
  type lol =
    | Complete of incomplete_file
    | Send_request of (sent_request * incomplete_file)
    | Pipeline_full_waiting_for_responses

  let next_get_request state (offer : incomplete_file) : t * sent_request list * incomplete_file list =
     (* complete_callback offer:
        - if stat .local_filename:
          - if .total_size ok:
            - if sha1 ok:
              = Done
            = Abort_incorrect_checksum (* at this point user may re-fetch *)
          = Abort_local_file_was_truncated
        = Abort_local_file_deleted             
     *)
    let pop_range ~ignore (incomp_file : incomplete_file) range tl : sent_request * incomplete_file =
      let offset = List.fold_left
                    (fun acc ->
                      fun sent_request ->
                      if sent_request.range_start = acc
                      then Int64.add acc chunk_size
                      else acc
                    )
                    (fst range)
                    ignore
      in
      let diff = Int64.sub (snd range) offset in
      if diff < chunk_size then
        { remote_filename = incomp_file.remote_filename
        ; request_id = "yo123"
        ; range_start = offset
        ; range_end = Int64.add offset diff
        }
      , {incomp_file with missing_ranges = tl}
      else
        { remote_filename = incomp_file.remote_filename
        ; request_id = "yo1234"
        ; range_start = offset
        ; range_end = Int64.(pred (add offset chunk_size))
        }
      , { incomp_file
          with missing_ranges =
               ( Int64.add offset chunk_size
               , (snd range)
               )::tl
        }
    in
    let update_incomplete_file this_file =
      begin match this_file.missing_ranges with
      | [] -> Complete this_file
      | range::tl ->
         (* the range list will either contain one range (0 - total_filesize)
            OR increments of chunk_size (from expired sent_get_requests),
            so since we will always be able to pop a chunk_size range,
            we only need to pass one at a time:
         *)
         let our_sent_requests =
           List.filter
             (function (sent : sent_request) -> sent.remote_filename = offer.remote_filename)
             state.sent_requests
         in
         if List.length our_sent_requests >= pipeline_size
         then Pipeline_full_waiting_for_responses
         else Send_request (pop_range ~ignore:our_sent_requests this_file range tl)
      end
    in
    let new_incomplete_files =
      let rec destructive_map (incomplete_lst : incomplete_file list) (request_lst : sent_request list) complete_lst =
        (function
         | [] ->
            incomplete_lst , request_lst, complete_lst
         | incom_file::tl when incom_file.sha1_hash = offer.sha1_hash ->
            begin match update_incomplete_file incom_file with
            | Pipeline_full_waiting_for_responses ->
               tl |> destructive_map
                 (incom_file::incomplete_lst)
                 request_lst
                 complete_lst
            | Send_request (send_request , incom_file) ->
               tl |> destructive_map
                 (incom_file::incomplete_lst)
                 (send_request::request_lst)
                 complete_lst
            | Complete file ->
               tl |> destructive_map
                 incomplete_lst
                 request_lst
                 (file::complete_lst)
            end
         | incom_file::tl ->
            tl |> destructive_map
                    (incom_file::incomplete_lst)
                    request_lst
                    complete_lst
        )
      in
      destructive_map [] [] [] state.incomplete_files
    in
    let nif , to_send , complete = new_incomplete_files in
    {state with incomplete_files = nif} , to_send , complete

  let create () = { sent_offers = [] ; received_offers = [] ; sent_requests = [] ; incomplete_files = [] }

  let receive_offer_request state (offer : Otr.Otrdata.offer_request) : t * bool =
     (* check if the offer is acceptable:
        - non-negative size
        - request-id that doesn't conflict with anything else
        - cool filename
        - nothing we're already fetching
      *)
     {state with received_offers = {remote_filename = offer.offer_path; local_filename = offer.offer_path; total_size = offer.file_length; sha1 = offer.sha1} :: state.received_offers} , true

  let list_received_offers state : string list =
    List.map
      (fun (offer : offer) -> offer.remote_filename)
      state.received_offers

  let receive_get_request state filename : offer option =
    (* check if we have offered [filename] to this contact *)
    let rec find = function
      | [] -> None
      | (offer : offer) :: _ when offer.remote_filename = filename ->
         Some offer
      | _ :: tl -> find tl
    in
    find (state.sent_offers : offer list)

  let receive_response state resp_request_id body :
        t * get_response_result
    =
    let rec find_sent_get = function
      | req::_ when req.request_id = resp_request_id ->
         if Int64.(sub req.range_end req.range_start
            <> of_int (String.length body))
         then Invalid_content
         else Get_response ({ filename = req.remote_filename; offset = req.range_start; content = body } : parsed_get_response)
      | _ :: tl -> find_sent_get tl
      | [] -> Unknown_request_id
    in
    (state
    , find_sent_get state.sent_requests)

  let _ , _ , _ , _ , _ =
    (next_get_request
    , receive_offer_request
    , list_received_offers
    , receive_get_request
    , receive_response)
end
