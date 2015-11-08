open Printf

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux (j-1) [] ;;

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

let modulo a b =
  let m = a mod b in
  if m < 0 then (m+b) else m;;

let help ()=
  print_string "Caesar Cipher\n";
  print_string "Usage: ./caesar [MODE] (KEY) SOURCE DESTINATION\n";
  print_newline ();
  print_string "Modes:\n";
  print_string "-e or --encrypt KEY\n";
  print_string "-d or --decrypt KEY\n";
  print_string "-b or --break\n";;

let caesar text key =
  implode (List.map (fun c -> Char.chr (modulo ((Char.code c) + key) 256)) (explode text));;

let cipher key src dist =
  let in_channel = open_in src in
  let out_channel = open_out dist in
  try
    while true do
      let line = input_line in_channel in

      let res = caesar line key in

      fprintf out_channel "%s\n" res;
    done;
  with End_of_file ->
  begin
    close_in in_channel;
    close_out out_channel;
  end;;

let break src dist =
  let in_channel = open_in src in
  let s = ref "" in
  let _ = (try
    while (String.length !s) < 100 do
      s := !s ^ (input_line in_channel);
    done;
  with _ -> ()) in
  let sub = try String.sub !s 0 100 with _ -> !s in
  close_in in_channel;

  printf "Choose the text which make sense.\n";
  let _ = read_line () in
  let brute text key =
    key,caesar text (-key)
  in
  let comp (xk,xt) (yk,yt) =
    let count x = (if (Char.code x) >= 32 && (Char.code x) < 127 then (if x = ' ' || ((Char.code x) >= 64 && (Char.code x) <= 122 && ((Char.code x) < 91 || (Char.code x) > 96)) then 5 else 1) else 0) in
    let xcount = List.fold_left
      (fun v x -> v + (count x))
      0 (explode xt) in
    let ycount = List.fold_left
      (fun v x -> v + (count x))
      0 (explode yt) in
    compare ycount xcount
  in
  let k = ref (-1) in
  let find (key,text) =
    if !k >= 0 then () else
    begin
      printf "%i) %s [y/N] " key (String.map (fun c -> if (Char.code c) < 32 then ' ' else c) text);
      let input = try Char.lowercase (List.hd (explode (read_line ()))) with _ -> 'n' in
      k := if input = 'y' then key else !k;
      ()
    end;
  in
  let _ = List.map find (List.sort comp (List.map (brute sub) (0--256))) in
  if !k >= 0 then
    let _ = cipher (-(!k)) src dist in
    printf "Great !\n";
  else
    printf "Sorry, we couldn't find it...\n";;

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
  | ("-e",key,src,dist) | ("--encrypt",key,src,dist) when src <> dist && key <> 0 ->
    (try cipher key src dist with _ -> printf "'%s' doesn't exist\n" src)
  | ("-d",key,src,dist) | ("--decrypt",key,src,dist) when src <> dist && key <> 0 ->
    (try cipher (-key) src dist with _ -> printf "'%s' doesn't exist\n" src)
  | ("-b",0,src,dist) | ("--break",0,src,dist) when src <> dist ->
    (try break src dist with _ -> printf "'%s' doesn't exist\n" src)
  | _ -> help ();
