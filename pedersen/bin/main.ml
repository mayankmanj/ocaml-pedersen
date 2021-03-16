
open Blake2s

let input = "Hello World"

let hash = blake2s (Bytes.of_string input) 32

let get_printable (ch : char) = Printf.sprintf "%02x" (int_of_char ch)

let printable_list = List.init (Bytes.length hash) (fun i -> get_printable (Bytes.get hash i))

let () = print_endline (String.concat "" printable_list)
