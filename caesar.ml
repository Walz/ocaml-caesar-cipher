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
  let n = in_channel_length in_channel in
  let text = Bytes.create n in
  really_input in_channel text 0 n;
  close_in in_channel;

  let explode_text = List.sort compare (explode text) in
  let freqList = List.filter (fun (xl,xn) -> (Char.code xl) > 15) (
    List.sort compare (List.fold_left
    (fun x c -> match x with
      | ((xl,xn) :: t) -> (if xl = c then (xl,xn+1) :: t else (c,1) :: (xl,xn) :: t)
      | _ -> failwith "error")
    [(List.hd explode_text,0)]
    explode_text)) in
  let (letter,freq) = List.hd freqList in

  let mostFreqLetters = [' ';'e';'a'] in

  let found = ref false in
  let test text key =
    if !found then (false,0) else
    let res = caesar text (-key) in
    printf "%s\nDoes it look ok ? (key=%i) [y/N] " res key;
    let a = explode (read_line ()) in
    found :=  (try (List.hd a) = 'y' with _ -> false);
    (!found,key)
  in

  let in_channel = open_in src in
  let s = ref "" in
  while (String.length !s) < 100 do
    s := !s ^ (input_line in_channel);
  done;
  let sub = try String.sub !s 0 100 with _ -> !s in
  close_in in_channel;

  let tests = try
    List.map (test sub) (List.map (fun x -> modulo ((Char.code letter) - (Char.code x)) 256) mostFreqLetters)
  with _ -> [(false,0)] in

  let b,k = try List.find (fun (x,y) -> x) tests with _ -> (false,0) in
  if b then
  begin
  let _ = cipher (-k) src dist in
  printf "Great !\n";
  end
  else
  begin
    printf "\n%s\n%s"
    "We are going to have to go brute force on this !"
    "We're going to display all the possibilities 15 at a times. If you find the right one just type it's number.";
    let _ = read_line () in
    let brute text key =
      key,caesar text (-key)
    in
    let found = ref false in
    let k = ref 0 in
    let rec rfor i n =
      if i > n then !found else
      if !found then !found else
      let possibilities = List.map (brute sub) ((i*15)--(i*15+15)) in
      let _ = List.map
        (fun (k,s) -> printf "%i: %s\n" k
          (String.map (fun c -> if (Char.code c) > 31 && (Char.code c) < 127 then c else ' ') s))
        possibilities
      in
      let a = try int_of_string (read_line ()) with _ -> -1 in
      if a >= i*15+15 then
      begin
        printf "You can see the future ?!\n";
        rfor i n
      end
      else
      begin
        k := a;
        found := (try a >= 0 with _ -> false);
        rfor (i+1) n
      end;
    in
    if rfor 0 16 then
      let _ = cipher (-(!k)) src dist in
      printf "It was maybe a bit overkill... But it worked !\n";
    else
      printf "Sorry, we couldn't find it...\n";
  end;;

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
