
open Pedersen

let input = "Hello World"

let hash = pedersenhash (Bytes.of_string input)

let get_printable (ch : char) = Printf.sprintf "%02x" (int_of_char ch)

let printable_list = List.init (Bytes.length hash) (fun i -> get_printable (Bytes.get hash i))

let () = print_endline (String.concat "" printable_list)
