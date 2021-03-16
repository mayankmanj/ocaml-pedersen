open Stdint

type digest = {
  mutable h  : uint32 array;  (* current chain value *)
  mutable t  : uint32 array;  (* message bytes counter *)
  mutable f  : uint32 array;  (* finalization flags *)
  mutable x  : uint8 array;    (* buffer for data not yet compressed *)
  mutable nx : int;           (* number of bytes in buffer *)

	mutable ih         : uint32 array;  (* initial chain value (after config) *)
  mutable paddedKey  : uint8 array;   (* copy of key, padded with zeros *)
  mutable isKeyed    : bool;          (* indicates whether hash was keyed *)
	mutable size       : uint8;         (* digest size in bytes *)
	mutable isLastNode : bool           (* indicates processing of the last node in tree hashing *)
}

(* Tree represents parameters for tree hashing. *)
type tree = {
  fanout        : uint8;  (* fanout *)
  maxDepth      : uint8;  (* maximal depth *)
  leafSize      : uint32; (* leaf maximal byte length (0 for unlimited) *)
  nodeOffset    : uint64; (* node offset (0 for first, leftmost or leaf), max 2^48-1 *)
  nodeDepth     : uint8;  (* node depth (0 for leaves) *)
  innerHashSize : uint8;  (* inner hash byte length *)
  isLastNode    : bool    (* indicates processing of the last node of layer *)
}


(* Config is used to configure hash function parameters and keying.
 * All parameters are optional. *)
type config = {
	size : uint8;        (*.digest size (if zero, default size of 32 bytes is used) *)
  key  : uint8 array;   (* key for prefix-MAC *)
	salt : uint8 array;   (* salt (if < 8 bytes, padded with zeros) *)
  person : uint8 array; (* personalization (if < 8 bytes, padded with zeros) *)
  tree   : tree      (* parameters for tree hashing *)
}


let blockSize_int = 64  (* block size of algorithms *)
let blockSize = Uint32.of_int 64  (* block size of algorithms *)

let size = Uint8.of_int 32  (* maximum digest size *)

let saltSize = 8  (* maximum salt size *)

let personSize = 8  (* maximum personalization string size *)

let keySize = 32  (* maximum size of key *)

(* Initialization values *)
let iv : uint32 array = Array.map Uint32.of_int [|
    0x6a09e667; 0xbb67ae85; 0x3c6ef372; 0xa54ff53a;
	  0x510e527f; 0x9b05688c; 0x1f83d9ab; 0x5be0cd19;
  |]

let defaultTree = {
  fanout = Uint8.of_int 1;
  maxDepth = Uint8.of_int 1;
  leafSize = Uint32.of_int 0;
  nodeOffset = Uint64.of_int 0;
  nodeDepth = Uint8.of_int 0;
  innerHashSize = Uint8.of_int 0;
  isLastNode = false;
}

let defaultConfig : config = {
  size = size;
  key = [||];
  salt = [||];
  person = [||];
  tree = defaultTree
}
