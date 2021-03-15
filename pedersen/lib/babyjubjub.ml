let ( @$ ) f x = f x;;

let r = Z.of_string "21888242871839275222246405745257275088548364400416034343698204186575808495617";;

module Fr = Ff.MakeFp (struct
    let prime_order = r
  end);;

(* Parameters of twisted Edwards curve. *)
let a = Fr.of_string "168700";;
let d = Fr.of_string "168696";;


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

let generator_pt = {
  x = Fr.of_string "995203441582195749578291179787384436505546430278305826713579947235728471134";
  y = Fr.of_string "5472060717959818805561601436314318772137091100104008585924551046643952123905"
}

let base_pt = {
  x = Fr.of_string "5299619240641551281634865583518297030282874472190772894086521144482721001553";
  y = Fr.of_string "16950150798460657717958625567821834550301663161624707787222815936182638968203"
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
  let open Fr in
  let rec mul_helper a c (acc : Point.t) =
    if Z.equal c Z.zero then acc else
      let new_a = double_p a in
      let new_c = Z.shift_right c 1 in
      if Z.equal (Z.logand c Z.one) Z.zero then
        mul_helper new_a new_c acc
      else
        mul_helper new_a new_c (add_p acc a) in
 mul_helper a (Fr.to_z c) {x=Fr.zero; y=Fr.one}


let p1 : Point.t =
  {x = Fr.of_string "17777552123799933955779906779655732241715742912184938656739573121738514868268";
   y = Fr.of_string "2626589144620713026669568689430873010625803728049924121243784502389097019475"}

let p2 : Point.t =
  {x = Fr.of_string "16540640123574156134436876038791482806971768689494387082833631921987005038935";
   y = Fr.of_string "20819045374670962167435360035096875258406992893633759881276124905556507972311"}

let expected_sum : Point.t =
  {x = Fr.of_string "7916061937171219682591368294088513039687205273691143098332585753343424131937";
   y = Fr.of_string "14035240266687799601661095864649209771790948434046947201833777492504781204499"}

let sum_of_p12 = add_p p1 p2

let print_p (pt : Point.t) = Printf.printf "{x = %s;\ny = %s}" (Fr.to_string pt.x) (Fr.to_string pt.y)

let _ = print_p sum_of_p12
