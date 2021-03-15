
open Jubjub

type message_bits =
  {
    input : Bytes.t;
    mutable bit : int;
    mutable byte : int;
    mutable byte_num : int;
    length : int;
  }

let msg_bits_of_msg msg = {
  input = msg;
  bit = 256;
  byte = 0;
  byte_num = 0;
  length = Bytes.length msg
}

let rec getbit b =
  if b.bit = 256 then
    begin
      b.byte <- int_of_char (Bytes.get b.input (b.byte_num + 1));
      b.byte_num <- b.byte_num + 1;
      b.bit <- 1;
      getbit b
    end
  else
    let r = b.byte land b.bit > 0 in
    b.bit <- b.bit * 2;
    r

let getchunk b =
  if b.length = b.byte_num + 1 then
    None
  else
  let acc = ref 0 in
  for i = 0 to 2 do
    acc := !acc * 2;
    try
      if getbit b then
        acc := !acc + 1
    with
      Invalid_argument _ -> ()
  done;
  Some (Z.of_int !acc)

let getmajorchunk b =
  let rec helper acc n =
    if n = 0 then List.rev acc else
      match getchunk b with
      | Some v -> helper (v :: acc) (n-1)
      | None -> List.rev acc in
  match helper [] 63 with
  | [] -> None
  | v -> Some v

let enc_chunk num =
  if num > 7 then raise (Invalid_argument "Must be less than 8") else
    let s2 = num land 4 in
    let s1s0 = Z.of_int (num - s2) in
    if s2 > 0 then Z.neg s1s0 else s1s0

let enc_major_chunk chunk_lst =
  let open Fr in
  let sixteen = of_z (Z.of_int 16) in
  let rec helper acc lst mul =
    match lst with
    | [] -> acc
    | hd :: tl -> helper (acc + hd * mul) tl (mul * sixteen) in
  helper zero chunk_lst one

