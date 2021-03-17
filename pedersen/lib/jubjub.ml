let ( @$ ) f x = f x;;

let r = Z.of_string "52435875175126190479447740508185965837690552500527637822603658699938581184513";;

module Fr = Ff.MakeFp (struct
    let prime_order = r
  end);;

(* Parameters of twisted Edwards curve. *)
let a = Fr.of_string "-1";;
let d = Fr.( (-) ((of_string "10240") / of_string "10241"));;


let order = r;;
let sub_order = Fr.of_z @$ Z.shift_right r 3;;

type point = {
  x : Fr.t;
  y : Fr.t;
}

type projective_pt = {
  x : Fr.t;
  y : Fr.t;
  z : Fr.t;
}

let affine ({x; y; z} : projective_pt) =
  if Fr.is_zero z then
    raise @$ Failure "Can't be done!"
  else
    Fr.({ x = (x / z); y = (y / z) })

let projective ({x; y} : point) =
  {x=x; y=y; z=Fr.one}

module Point : sig
  type t
  val eq : t -> t ->bool
end with type t = point
= struct
  type t = point

  let eq (a :t)  (b:t) = (a.x = b.x) && (a.y = b.y)

end

module Projective : sig
  type t
  val eq : t -> t -> bool
  val add : t -> t -> t
  val double: t -> t
end with type t = projective_pt
= struct
  type t = projective_pt

  let eq a b =
    match Fr.is_zero a.z, Fr.is_zero b.z with
    | true, true -> false
    | true, false -> false
    | false, true -> false
    | _ -> Point.eq (affine a) (affine b);;


  (*******************************************************************************)
  (*
      A = Z1*Z2
      B = A2
      C = X1*X2
      D = Y1*Y2
      E = d*C*D
      F = B-E
      G = B+E
      X3 = A*F*((X1+Y1)*(X2+Y2)-C-D)
      Y3 = A*G*(D-a*C)
      Z3 = F*G
  *)
  (*******************************************************************************)
  let add {x=x1; y=y1; z=z1} {x=x2; y=y2; z=z2} =
    let open Fr in
    let a_ = z1 * z2 in
    let b_ = a_ * a_ in
    let c_ = x1 * x2 in
    let d_ = y1 * y2 in
    let e_ = d * c_ * d_ in
    let f_ = b_ + ((-) e_) in
    let g_ = b_ + e_ in
    let x3 = a_ * f_ * ((x1 + y1) * (x2 + y2) + ((-) c_) + ((-) d_)) in
    let y3 = a_ * g_ * (d_ + ((-) a * c_)) in
    let z3 = f_ * g_ in
    {x = x3; y = y3; z = z3}

  let double {x; y; z} =
    let open Fr in
    let b_ = (x + y) * (x + y) in
    let c_ = x * x in
    let d_ = y * y in
    let e_ = a * c_ in
    let f_ = e_ + d_ in
    let h_ = z * z in
    let j_ = f_ + ((-) ((one + one) * h_)) in
    let x' = (b_ + ((-) c_) + ((-) d_)) * j_ in
    let y' = f_ * (e_ + ((-) d_)) in
    let z' = f_ * j_ in
    {x = x'; y = y'; z = z'}
end

let add_p (a : Point.t) (b : Point.t) = affine (Projective.add (projective a) (projective b))
let double_p (a : Point.t) = affine (Projective.double (projective a))

let mul_scalar (a : Point.t) (c : Fr.t) =
  let rec mul_helper a c (acc : Point.t) =
    if Z.equal c Z.zero then acc else
      let new_a = double_p a in
      let new_c = Z.shift_right c 1 in
      if Z.equal (Z.logand c Z.one) Z.zero then
        mul_helper new_a new_c acc
      else
        mul_helper new_a new_c (add_p acc a) in
 mul_helper a (Fr.to_z c) {x=Fr.zero; y=Fr.one}

let repr ({x=u; y=v} : Point.t) =
  let u' = (Z.to_int Fr.(to_z u) mod 2) * 128 in
  let buffer = Bytes.make 32 '\000' in
  let v_z = ref (Fr.to_z v) in
  for i = 0 to 31 do
    let byte = (Z.to_int Z.(logand !v_z (of_int 255))) in
    begin
      match i with
      | 31 -> Bytes.set buffer i (char_of_int (byte + u'))
      | _  -> Bytes.set buffer i (char_of_int byte)
    end;
    v_z := Z.shift_right !v_z 8;
  done;
  buffer

let abst buf : Point.t option =
  let last_byte = int_of_char (Bytes.get buf 31) in
  let u' = last_byte / 128 in
  let v_last = last_byte - u' * 128 in
  let v = Bytes.sub buf 0 32 in
  Bytes.set v 31 (char_of_int v_last);
  let v_val = ref Z.zero in
  let mult = ref Z.one in
  for i = 0 to 31 do
    let byte = Z.of_int (int_of_char (Bytes.get v i)) in
    v_val := Z.(!v_val + !mult * byte);
    mult := Z.(!mult * (of_int 256));
  done;
  if Z.(gt !v_val r) then None
  else
    let v = Fr.of_z !v_val in
    if Fr.is_zero Fr.(a + (-) d * v * v) then None
    else
      let u2 = Fr.((one + (-) v * v) / (a + (-) d * v * v)) in
      let u = Fr.sqrt_opt u2 in
      match u with
      | None -> None
      | Some u -> if Z.of_int u' = Z.(logand (Fr.to_z u) one) then
          Some {x=u; y=v}
        else
            Some {x= Fr.(-) u; y=v}

let extract ({x; _} : Point.t) =
  let out = Bytes.make 32 '\000' in
  let z_of_x = ref (Fr.to_z x) in
  for i = 0 to 31 do
    Bytes.set out i (char_of_int Z.(to_int (logand !z_of_x (of_int 255))));
    z_of_x := Z.shift_right !z_of_x 8;
  done;
  out
