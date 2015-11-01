open Printf

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> Bytes.set res i c ; imp (i + 1) l in
  imp 0 l;;

let help ()=
  print_string "Caesar Cipher\n";
  print_string "Usage: ./caesar [MODE] KEY SOURCE DESTINATION\n";
  print_newline ();
  print_string "Modes:\n";
  print_string "-e or --encrypt\n";
  print_string "-d or --decrypt\n";;

let modulo a b =
  let m = a mod b in
  if m < 0 then (-m) else m;;

let cipher key src dist =
  let in_channel = open_in src in
  let out_channel = open_out dist in
  try
    while true do
      let line = input_line in_channel in

      let res = implode (List.map (fun c -> Char.chr (modulo ((Char.code c) + key) 255)) (explode line)) in

      fprintf out_channel "%s\n" res;
    done;
  with End_of_file ->
  begin
    close_in in_channel;
    close_out out_channel;
  end;;

let _ =
  let (mode,key,src,dist) =
    try
      (Sys.argv.(1), int_of_string Sys.argv.(2), Sys.argv.(3), Sys.argv.(4))
    with
      _ -> ("0",0,"0","0")
    in

  match (mode,key,src,dist) with
  | ("-e",key,src,dist) | ("--encrypt",key,src,dist) when src <> dist ->
    (try cipher key src dist with _ -> printf "'%s' doesn't exist\n" src)
  | ("-d",key,src,dist) | ("--decrypt",key,src,dist) when src <> dist ->
    (try cipher (-key) src dist with _ -> printf "'%s' doesn't exist\n" src)
  | _ -> help ();
