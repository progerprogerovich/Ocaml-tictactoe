open Lwt.Infix

let create_board () = 
  Array.init 9 (fun i -> Char.chr (Char.code '1' + i))

let render_board b =
  Printf.sprintf 
    "\n %c | %c | %c \n---|---|---\n %c | %c | %c \n---|---|---\n %c | %c | %c \n\n"
    b.(0) b.(1) b.(2) b.(3) b.(4) b.(5) b.(6) b.(7) b.(8)

let check_win b =
  let wins = [
    (0,1,2); (3,4,5); (6,7,8);
    (0,3,6); (1,4,7); (2,5,8);
    (0,4,8); (2,4,6)
  ] in
  List.exists (fun (i, j, k) -> b.(i) = b.(j) && b.(j) = b.(k)) wins

let check_draw b =
  Array.for_all (fun c -> c = 'X' || c = 'O') b

type player = {
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
  mark: char;
}

let waiting_player : (Lwt_io.input_channel * Lwt_io.output_channel * unit Lwt.u) option ref = ref None

let send p msg = 
  Lwt_io.write_line p.oc msg >>= fun () -> Lwt_io.flush p.oc

let send_both p1 p2 msg = 
  Lwt.join [send p1 msg; send p2 msg]

let rec game_loop board p_turn p_other =
  let board_str = render_board board in
  send_both p_turn p_other board_str >>= fun () ->
  send p_turn "Your turn (enter 1-9):" >>= fun () ->
  send p_other "Waiting for opponent's move..." >>= fun () ->
  Lwt_io.read_line_opt p_turn.ic >>= function
  | None ->
      send p_other "Opponent disconnected. Game over." 
  | Some line ->
      let move =
        try
          let n = int_of_string (String.trim line) - 1 in
          if n >= 0 && n <= 8 && board.(n) <> 'X' && board.(n) <> 'O' 
          then Some n else None
        with _ -> None
      in
      
      match move with
      | Some n ->
          board.(n) <- p_turn.mark;
          if check_win board then
            send_both p_turn p_other (render_board board) >>= fun () ->
            send p_turn "Congratulations! You WIN!\n" >>= fun () ->
            send p_other "Sorry, you LOSE.\n"
          else if check_draw board then
            send_both p_turn p_other (render_board board) >>= fun () ->
            send_both p_turn p_other "Draw game!\n"
          else
            game_loop board p_other p_turn
      | None ->
          send p_turn "Invalid move! Try again." >>= fun () ->
          game_loop board p_turn p_other

let start_game (ic1, oc1) (ic2, oc2) =
  let p1 = { ic = ic1; oc = oc1; mark = 'X' } in
  let p2 = { ic = ic2; oc = oc2; mark = 'O' } in
  
  Lwt.catch
    (fun () ->
       send p1 "Game started! You are [X]." >>= fun () ->
       send p2 "Game started! You are [O]." >>= fun () ->
       let board = create_board () in
       game_loop board p1 p2
    )
    (fun _exn ->
       Lwt.return_unit
    )

let handle_connection _addr (ic, oc) =
  send {ic; oc; mark=' '} "Welcome to PVP Tic-Tac-Toe!" >>= fun () ->
  match !waiting_player with
  | None ->
      let p, u = Lwt.wait () in
      waiting_player := Some (ic, oc, u);
      send {ic; oc; mark=' '} "Waiting for the second player..." >>= fun () ->
      p 
  | Some (wait_ic, wait_oc, wait_u) ->
      waiting_player := None;
      start_game (wait_ic, wait_oc) (ic, oc) >>= fun () ->
      Lwt.wakeup wait_u ();
      Lwt.return_unit

let create_server port =
  let listen_address = Unix.inet_addr_any in
  let sockaddr = Unix.ADDR_INET (listen_address, port) in
  
  let _server = Lwt_io.establish_server_with_client_address sockaddr handle_connection in
  
  Lwt_io.printf "Server started on port %d. Waiting for connections...\n" port >>= fun () ->
  fst (Lwt.wait ())

let () =
  let port = 9000 in
  Lwt_main.run (create_server port)