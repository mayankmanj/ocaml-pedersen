
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
  for _ = 0 to 2 do
    acc := !acc * 2;
    try
      if getbit b then
        acc := !acc + 1
    with
      Invalid_argument _ -> ()
  done;
  Some (!acc)

let getmajorchunk b =
  let rec helper acc n =
    if n = 0 then List.rev acc else
      match getchunk b with
      | Some v -> helper (v :: acc) (n-1)
      | None -> List.rev acc in
  match helper [] 63 with
  | [] -> None
  | v -> Some (List.map (fun x -> Fr.of_z (Z.of_int x)) v)

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

let urs = Hex.to_bytes (`Hex "096b36a5804bfacef1691e173c366a47ff5ba84a44f26ddd7e8d9f79d5b42df0")

let group_hash d msg =
  let joined = Bytes.cat urs msg in
  let blake_hash = Blake2s.blake2s ~key:d joined in
  match Jubjub.abst blake_hash with
  | None -> None
  | Some pt ->
      let ({x; y} as q : Jubjub.Point.t) = Jubjub.mul_scalar pt (Fr.of_z (Z.of_int 8)) in
      if x = Fr.zero && y = Fr.one then None
        else Some q

let find_group_hash d m =
  let length = Bytes.length m in
  let buf = Bytes.make (length+1) '\000' in
  Bytes.blit m 0 buf 0 length;
  let rec helper i =
    Bytes.set buf (length) (char_of_int i);
    match group_hash d buf with
    | None -> helper (i + 1)
    | Some v -> v
  in
  helper 0

let hash_to_point d m =
  let msg_bits = msg_bits_of_msg m in
  let rec helper acc i =
    let buf_i = Bytes.make 4 '\000' in
    Stdint.Uint32.(to_bytes_little_endian (of_int Stdlib.(i-1)) buf_i 0);
    let gh = find_group_hash d buf_i in
    match getmajorchunk msg_bits with
    | None -> acc
    | Some lst -> helper Jubjub.(add_p acc (mul_scalar gh (enc_major_chunk lst))) (i+1)
  in
  helper {x=Fr.zero; y=Fr.one} 1


let pedersenhash ?(d=Bytes.make 8 '\000') m =
  Jubjub.extract (hash_to_point d m)

