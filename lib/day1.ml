let readlines file = Core.In_channel.read_lines file

let isNum c =
  (int_of_char c)>47 && (int_of_char c)<58

let str_to_list s = List.init (String.length s) (String.get s)

let first_and_last l =
  [List.hd l; (List.rev l |> List.hd)]

let concat_chars_in_list char_list =
  String.concat "" (List.map Char.escaped char_list)

let get_int str =
  str 
  |> str_to_list 
  |> List.filter (fun x -> isNum x)
  |> first_and_last
  |> concat_chars_in_list 
  |> int_of_string

let rec sum = function
  | [] -> 0
  | h :: t -> get_int h + sum t
