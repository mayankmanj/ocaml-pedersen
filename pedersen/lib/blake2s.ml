
(* Literal translation of reference blake2s implementation from
   https://github.com/BLAKE2/BLAKE2 *)

open Stdint


let iv = Array.map Uint32.of_int
    [|
      0x6A09E667; 0xBB67AE85; 0x3C6EF372; 0xA54FF53A;
      0x510E527F; 0x9B05688C; 0x1F83D9AB; 0x5BE0CD19
|]

let sigma = [|
      [|  0;  1;  2;  3;  4;  5;  6;  7;  8;  9; 10; 11; 12; 13; 14; 15 |] ;
      [| 14; 10;  4;  8;  9; 15; 13;  6;  1; 12;  0;  2; 11;  7;  5;  3 |] ;
      [| 11;  8; 12;  0;  5;  2; 15; 13; 10; 14;  3;  6;  7;  1;  9;  4 |] ;
      [|  7;  9;  3;  1; 13; 12; 11; 14;  2;  6;  5; 10;  4;  0; 15;  8 |] ;
      [|  9;  0;  5;  7;  2;  4; 10; 15; 14;  1; 11; 12;  6;  8;  3; 13 |] ;
      [|  2; 12;  6; 10;  0; 11;  8;  3;  4; 13;  7;  5; 15; 14;  1;  9 |] ;
      [| 12;  5;  1; 15; 14; 13;  4; 10;  0;  7;  6;  3;  9;  2;  8; 11 |] ;
      [| 13; 11;  7; 14; 12;  1;  3;  9;  5;  0; 15;  4;  8;  6;  2; 10 |] ;
      [|  6; 15; 14;  9; 11;  3;  0;  8; 12;  2; 13;  7;  1;  4; 10;  5 |] ;
      [| 10;  2;  8;  4;  7;  6;  1;  5; 15; 11;  9; 14;  3; 12; 13 ; 0 |]
    |]

let blockbytes = 64

let outbytes   = 32

let keybytes   = 32

let saltbytes  = 8

let personalbytes = 8

type state = {
  mutable h : uint32 array;
  mutable t : uint32 array;
  mutable f : uint32 array;
  mutable buf : bytes;
  mutable buflen : int;
  mutable outlen : int;
  mutable last_node : uint8
}

type param = {
digest_length : uint8; (* 0 *)
key_length : uint8; (* 1 *)
fanout : uint8; (* 2 *)
depth : uint8; (* 3 *)
leaf_length : uint32; (* 4 *)
node_offset : uint32; (* 8 *)
xof_length : uint16; (* 12 *)
node_depth : uint8; (* 14 *)
inner_length : uint8; (* 15 *)
salt : uint8 array; (* 16 *)
personal : uint8 array; (* 24 *)
}

let set_lastnode (state : state) =
  state.f.(1) <- Uint32.of_int 0xffffffff

let is_lastblock (state : state) =
  state.f.(0) != Uint32.zero

let set_lastblock (s : state) =
  if s.last_node != Uint8.zero then set_lastnode s;
  s.f.(0) <- Uint32.of_int 0xffffffff

let increment_counter s inc =
  let open Uint32 in
  s.t.(0) <- s.t.(0) + inc;
  if s.t.(0) < inc then s.t.(1) <- s.t.(1) + one

let init0 (s : state) =
  s.h <- Array.copy iv

let param_to_uint32_buffer (p : param) =
  let buffer = Bytes.make 32 '\000' in
  Uint8.to_bytes_little_endian p.digest_length buffer 0;
  Uint8.to_bytes_little_endian p.key_length buffer 1;
  Uint8.to_bytes_little_endian p.fanout buffer 2;
  Uint8.to_bytes_little_endian p.depth buffer 3;
  Uint32.to_bytes_little_endian p.leaf_length buffer 4;
  Uint32.to_bytes_little_endian p.node_offset buffer 8;
  Uint16.to_bytes_little_endian p.xof_length buffer 12;
  Uint8.to_bytes_little_endian p.node_depth buffer 14;
  Uint8.to_bytes_little_endian p.inner_length buffer 15;
  for i = 0 to saltbytes - 1 do
    Uint8.to_bytes_little_endian p.salt.(i) buffer (16+i);
  done;
  for i = 0 to personalbytes - 1 do
    Uint8.to_bytes_little_endian p.salt.(i) buffer (24+i);
  done;
  buffer

let init_param (s : state) (p : param) =
  init0 s;
  let buffer = param_to_uint32_buffer p in
  for i = 0 to 7 do
    s.h.(i) <- Uint32.logxor s.h.(i) (Uint32.of_bytes_little_endian buffer (4*i));
  done;
  s.outlen <- Uint8.to_int p.digest_length

let init (s : state) outlen =
  if outlen = 0 || outlen > outbytes then
    raise (Invalid_argument "check outlen");
  let p : param = {
    digest_length = Uint8.of_int outlen;
    key_length = Uint8.zero;
    fanout = Uint8.one;
    depth = Uint8.one;
    leaf_length = Uint32.zero;
    node_offset = Uint32.zero;
    xof_length = Uint16.zero;
    node_depth = Uint8.zero;
    inner_length = Uint8.zero;
    salt = Array.make saltbytes Uint8.zero;
    personal = Array.make personalbytes Uint8.zero
  } in
  init_param s p

let rotate_right a n =
  let open Uint32 in
  let rs = shift_right a n in
  let carry = shift_left (logand a ((shift_left one n) - one)) Stdlib.(32 - n) in
  logor rs carry

let g_ r i v a b c d m =
  let open Uint32 in
  v.(a) <- v.(a) + v.(b) + m.(sigma.(r).(Stdlib.(2*i + 0)));
  v.(d) <- rotate_right (logxor v.(d) v.(a)) 16;
  v.(c) <- v.(c) + v.(d);
  v.(b) <- rotate_right (logxor v.(b) v.(c)) 12;
  v.(a) <- v.(a) + v.(b) + m.(sigma.(r).(Stdlib.(2*i+1)));
  v.(d) <- rotate_right (logxor v.(d) v.(a)) 8;
  v.(c) <- v.(c) + v.(d);
  v.(b) <- rotate_right (logxor v.(b) v.(c)) 7 [@@inline]

let round r v m =
  g_ r 0 v 0 4 8 12 m;
  g_ r 1 v 1 5 9 13 m;
  g_ r 2 v 2 6 10 14 m;
  g_ r 3 v 3 7 11 15 m;
  g_ r 4 v 0 5 10 15 m;
  g_ r 5 v 1 6 11 12 m;
  g_ r 6 v 2 7 8 13 m;
  g_ r 7 v 3 4 9 14 m [@@inline]

let compress (s:state) inp =
  let m = Array.make 16 Uint32.zero in
  let v = Array.make 16 Uint32.zero in
  let buffer = Bytes.copy inp in
  for i = 0 to 15 do
    m.(i) <- Uint32.of_bytes_little_endian buffer (4*i);
  done;
  for i = 0 to 7 do
    v.(i) <- s.h.(i);
  done;
  v.(8) <- iv.(0);
  v.(9) <- iv.(1);
  v.(10) <- iv.(2);
  v.(11) <- iv.(3);
  v.(12) <- Uint32.logxor s.t.(0) iv.(4);
  v.(13) <- Uint32.logxor s.t.(1) iv.(5);
  v.(14) <- Uint32.logxor s.f.(0) iv.(6);
  v.(15) <- Uint32.logxor s.f.(1) iv.(7);
  for i = 0 to 9 do
    round i v m;
  done;
  for i = 0 to 7 do
    s.h.(i) <- Uint32.logxor s.h.(i) (Uint32.logxor v.(i) v.(i+8))
  done


let update (s:state) pin inlen =
  let inp = ref pin in
  let inlen = ref inlen in
  if !inlen > 0 then
    begin
      let left = s.buflen in
      let fill = blockbytes - left in
      if !inlen > fill then
        begin
          s.buflen <- 0;
          Bytes.blit !inp 0 s.buf left fill;
          increment_counter s (Uint32.of_int blockbytes);
          compress s s.buf;
          inp := Bytes.sub !inp fill (Bytes.length !inp - fill);
          inlen := !inlen - fill;
          while !inlen > blockbytes do
            increment_counter s (Uint32.of_int blockbytes);
            compress s !inp;
            inp := Bytes.sub !inp blockbytes (Bytes.length !inp - blockbytes);
            inlen := !inlen - blockbytes;
          done
        end;
      Bytes.blit !inp 0 s.buf s.buflen !inlen;
      s.buflen <- s.buflen + !inlen;
    end

let init_key (s : state) outlen key keylen =
  if outlen = 0 || (outlen > outbytes) then
    raise (Invalid_argument "check outlen");
  if Bytes.length key = 0 || keylen = 0 || keylen > keybytes then
    raise (Invalid_argument "check key");
  let p : param= {
    digest_length = Uint8.of_int outlen;
    key_length = Uint8.of_int keylen;
    fanout = Uint8.one;
    depth = Uint8.one;
    leaf_length = Uint32.zero;
    node_offset = Uint32.zero;
    xof_length = Uint16.zero;
    node_depth = Uint8.zero;
    inner_length = Uint8.zero;
    salt = Array.make saltbytes Uint8.zero;
    personal = Array.make personalbytes Uint8.zero
  } in
  init_param s p;
  let block = Bytes.make blockbytes '\000' in
  Bytes.blit key 0 block 0 keylen;
  update s block blockbytes;
  Bytes.fill block 0 blockbytes '\000'

let final s out outlen =
  let buffer = Bytes.make outbytes '\000' in
  if outlen < s.outlen then raise (Invalid_argument "check outlen!");
  if is_lastblock s then raise (Invalid_argument "is_lastblock!");
  increment_counter s (Uint32.of_int s.buflen);
  set_lastblock s;
  Bytes.fill s.buf s.buflen (Bytes.length s.buf - s.buflen) '\000';  (* Padding *)
  compress s s.buf;
  for i = 0 to 7 do  (* Output full hash to temp buffer *)
    Uint32.to_bytes_little_endian s.h.(i) buffer (4*i);
  done;
  Bytes.blit buffer 0 out 0 outlen;
  Bytes.fill buffer 0 outbytes '\000'  (* securly clear bytes *)

let blake2s ?(key=Bytes.empty) ?(outlen=outbytes) inp =
  (* verify parameters *)
  let inlen = Bytes.length inp in
  let keylen = Bytes.length key in
  let s : state = {
    h = Array.make 8 Uint32.zero;
    t = Array.make 2 Uint32.zero;
    f = Array.make 2 Uint32.zero;
    buf = Bytes.make blockbytes '\000';
    buflen = 0;
    outlen = outlen;
    last_node = Uint8.zero;
  } in
  if outlen > outbytes then raise (Invalid_argument "outlen > outbytes");
  if keylen > keybytes then raise (Invalid_argument "keylen > keybytes");
  if keylen > 0 then init_key s outlen key keylen
  else init s outlen;
  update s inp inlen;
  let out = Bytes.make outlen '\000' in
  final s out outlen;
  out
