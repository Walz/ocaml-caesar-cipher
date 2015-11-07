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
  print_string "Usage: ./caesar [MODE] (KEY) SOURCE DESTINATION\n";
  print_newline ();
  print_string "Modes:\n";
  print_string "-e or --encrypt KEY\n";
  print_string "-d or --decrypt KEY\n";
  print_string "-b or --break\n";;

let modulo a b =
  let m = a mod b in
  if m < 0 then (m+b) else m;;

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

let break src dist =
  let in_channel = open_in src in
  let out_channel = open_out dist in
  let n = in_channel_length in_channel in
  let text = Bytes.create n in
  really_input in_channel text 0 n;
  let explode_text = List.sort compare (explode text) in
  let freqList = List.filter (fun (xl,xn) -> (Char.code xl) > 15) (
    List.sort compare (List.fold_left
    (fun x c -> match x with
      | ((xl,xn) :: t) -> (if xl = c then (xl,xn+1) :: t else (c,1) :: (xl,xn) :: t)
      | _ -> failwith "error")
    [(List.hd explode_text,0)]
    explode_text)) in
  let (letter,freq) = List.hd freqList in
  printf "%i of '%c' - %i\n" freq letter (Char.code letter);
  printf "key = %i\n" ((Char.code letter) - (Char.code ' '));
  close_in in_channel;
  close_out out_channel;;

let _ =
  let (mode,key,src,dist) =
    try
      (Sys.argv.(1), int_of_string Sys.argv.(2), Sys.argv.(3), Sys.argv.(4))
    with
      _ ->
      try
        (Sys.argv.(1), 0, Sys.argv.(2), Sys.argv.(3))
      with _ -> ("0",0,"0","0")
    in

  match (mode,key,src,dist) with
  | ("-e",key,src,dist) | ("--encrypt",key,src,dist) when src <> dist ->
    (try cipher key src dist with _ -> printf "'%s' doesn't exist\n" src)
  | ("-d",key,src,dist) | ("--decrypt",key,src,dist) when src <> dist ->
    (try cipher (-key) src dist with _ -> printf "'%s' doesn't exist\n" src)
  | ("-b",0,src,dist) | ("--break",0,src,dist) when src <> dist ->
    (try break src dist with _ -> printf "'%s' doesn't exist\n" src)
  | _ -> help ();
