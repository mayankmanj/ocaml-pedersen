(* Literal translation of Blake2s implementation in Go --
 *   https://github.com/dchest/blake2s
 *)

open Stdint

open Blake2sTypes

let verifyConfig c =
  if c.size > size then
    raise (Invalid_argument "digest size is too large.");
  if Array.length c.key > keySize then
    raise (Invalid_argument "key is too large.");
  if Array.length c.salt > saltSize then
    raise (Invalid_argument "salt is too large.");
  if Array.length c.person > personSize then
    (* Smaller personalization is okay; it will be padded with zeros. *)
    raise (Invalid_argument "personalization is too large.");
  if c.tree.innerHashSize > size then
    raise (Invalid_argument "Incorrect tree inner hash size.");
  if c.tree.nodeOffset > Uint64.((shift_left one 48) - one) then
    raise (Invalid_argument "tree node offset is too large")


let write_d d (p : uint8 array) =
  let mut_p = ref p in
  let left = ref (blockSize_int - d.nx) in
  if Array.length !mut_p > !left then
    begin
      print_endline "here";
      Array.blit !mut_p 0 d.x d.nx !left;
      let new_p = Array.make (Array.length !mut_p - !left) Uint8.zero in
      Array.blit !mut_p !left new_p 0 (Array.length p - !left);
      mut_p := new_p;
      Block.blocks d d.x;
      d.nx <- 0
    end;
  while Array.length !mut_p > blockSize_int do
      print_endline "here";
      let n = ref ((Array.length !mut_p) land (lnot (blockSize_int -1))) in
      if !n = Array.length !mut_p then n := !n - blockSize_int;
      Block.blocks d (Array.sub !mut_p 0 !n);
      let new_p = Array.sub !mut_p !n (Array.length !mut_p) in
      mut_p := new_p;
  done;
  d.x <- Array.append d.x !mut_p;
  d.nx <- d.nx + (Array.length !mut_p);
  Printf.printf "%d\n" d.nx


let initialize (d : digest) (c : config) =
  let p = Array.make blockSize_int (Uint8.of_int 0) in
  p.(0) <- c.size;
  p.(1) <- Uint8.of_int (Array.length c.key);
  Array.blit c.salt 0 p 16 (Array.length c.salt);
  Array.blit c.person 0 p 24 (Array.length c.person);
  p.(2) <- c.tree.fanout;
  p.(3) <- c.tree.maxDepth;
  let buffer = Bytes.make 8 (char_of_int 0) in
  Uint32.to_bytes_little_endian c.tree.leafSize buffer 0;
  p.(4) <- Uint8.of_int (int_of_char (Bytes.get buffer 0));
  p.(5) <- Uint8.of_int (int_of_char (Bytes.get buffer 1));
  p.(6) <- Uint8.of_int (int_of_char (Bytes.get buffer 2));
  p.(7) <- Uint8.of_int (int_of_char (Bytes.get buffer 3));
  Uint64.to_bytes_little_endian c.tree.nodeOffset buffer 0;
  p.(8) <- Uint8.of_int (int_of_char (Bytes.get buffer 0));
  p.(9) <- Uint8.of_int (int_of_char (Bytes.get buffer 1));
  p.(10) <- Uint8.of_int (int_of_char (Bytes.get buffer 2));
  p.(11) <- Uint8.of_int (int_of_char (Bytes.get buffer 3));
  p.(12) <- Uint8.of_int (int_of_char (Bytes.get buffer 4));
  p.(13) <- Uint8.of_int (int_of_char (Bytes.get buffer 5));
  p.(14) <- c.tree.nodeDepth;
  p.(15) <- c.tree.innerHashSize;
  d.size <- c.size;
  d.t <- Array.make 2 (Uint32.of_int 0);
  d.f <- Array.make 2 (Uint32.of_int 0);
  for i = 0 to 7 do
    for j = 0 to 3 do
      let value = p.(4*i+j) in
      Bytes.set buffer j Uint8.(char_of_int (to_int value));
    done;
    let value = Uint32.of_bytes_little_endian buffer 0 in
    d.h.(i) <- Uint32.(logxor iv.(i) value);
  done;
  if c.tree.isLastNode then
    d.isLastNode <- true;
  if Array.length c.key > 0 then
    begin
      d.paddedKey <- Array.copy c.key;
      write_d d d.paddedKey;
      d.isKeyed <- true;
    end;
  d.ih <- Array.copy d.h

(* New256 returns a new hash.Hash computing the BLAKE2s 32-byte checksum. *)
  let new256 () =
    let d = {
      h = Array.make 8 Uint32.zero;
      t = [||];
      f = Array.map Uint32.of_int [|0; 0|];
      x = [||];
      nx = 0;
      ih = Array.map Uint32.of_int [|0; 0|];
      paddedKey = [||]; isKeyed = false; size = Uint8.zero; isLastNode = false
    } in
    initialize d defaultConfig;
    d

(* Reset resets the state of digest to the initial state
 * after configuration and keying. *)
let reset (d : digest) =
  d.h <- Array.copy d.ih;
  d.t.(0) <- Uint32.zero;
	d.t.(1) <- Uint32.zero;
	d.f.(0) <- Uint32.zero;
	d.f.(1) <- Uint32.zero;
	d.nx <- 0;
	if d.isKeyed then
   write_d d d.paddedKey;
   ()

let checkSum (d : digest) =
  if d.isKeyed then
    for i = 0 to Array.length d.paddedKey - 1 do
      d.paddedKey.(i) <- Uint8.zero
    done;
  let dec = Uint32.(blockSize - of_int d.nx) in
  if d.t.(0) < dec then
    d.t.(1) <- Uint32.(d.t.(1) - one);
  d.t.(0) <- Uint32.(d.t.(0) - dec);
  (* Pad buffer with zeros. *)
  for i = d.nx to Array.length d.x - 1 do
    d.x.(i) <- Uint8.zero
  done;
  (* Set last block flag. *)
  d.f.(0) <- Uint32.of_int 0xffffffff;
  if d.isLastNode then
    d.f.(1) <- Uint32.of_int 0xffffffff;
  Block.blocks d (Array.copy d.x);
  let buffer = Bytes.make (Uint8.to_int d.size) (char_of_int 0) in
  for i = 0 to (Uint8.to_int d.size - 1) / 4 do
    Uint32.to_bytes_little_endian d.h.(i) buffer (4*i);
  done;
  buffer


(* Sum256 returns a 32-byte BLAKE2s hash of data. *)
let sum256 (data : bytes) =
  let d = new256 () in
  let length = Bytes.length data in
  let arr = Array.init length (fun i -> Uint8.of_int (int_of_char (Bytes.get data i))) in
  write_d d arr;
  checkSum d
