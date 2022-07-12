module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *) 
  val mul_inv : t -> t          (* multiplicative inverse *)
end


module type RationalField = sig
  include Field with type t = int * int
  type t = int * int          (* rationals are represented as pairs of int *)
  exception Bad_rational of string
  val standard_form : t -> t  (* standard from of a rational number *)
  val to_float : t -> float   (* decimal expansion *)
  val from_int : int -> t     (* integer to rational conversion *)          
end


module type GaussianRationalField = sig
  include Field with type t = (int * int) * (int * int)
    (* Gaussian rationals are represented as pairs of rationals *)
  exception Division_by_zero of string
  val from_rational : (int * int ) -> t   (* rational to complex *)     
  val conj : t -> t                       (* conjugate *)
  val re : t -> (int * int)               (* real part *)
  val im : t -> (int * int)               (* imaginary part *)
end




module Rationals : RationalField = struct
  type t = int * int
  exception Bad_rational of string
  let zero = (0,1)
  let one = (1,1)
  
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
        
  let standard_form (n,d) = 
    if (d<0 && n>0) then (-n/(gcd n (-d)) ,-d/(gcd n (-d)))
    else if (d<0 && n<0) then ((-n)/(gcd (-n) (-d)) ,(-d)/(gcd (-n) (-d)))
    else if (d>0 && n<0) then (n/(gcd (-n) d) ,d/(gcd (-n) d))
    else (n/(gcd n d) ,d/(gcd n d))
  
  let compare (r1,i1) (r2,i2) = 
    match (standard_form (r1,i1), standard_form (r2,i2)) with |
      ((a,b),(c,d))->(
        if a*d-c*b>0 then 1 
        else (if a*d-b*c<0 then -1
              else 0))

  let add (r1,i1) (r2,i2) = 
    if (i1*i2 = 0) then raise (Bad_rational "stupid denominator") 
    else standard_form (r1*i2 + r2*i1,i1*i2)

  let sub (r1,i1) (r2,i2) = 
    if (i1*i2 = 0) then raise (Bad_rational "stupid denominator") 
    else standard_form (r1*i2 - r2*i1,i1*i2)
  
  let mul (r1,i1) (r2,i2) = 
    if (i1*i2 = 0) then raise (Bad_rational "stupid denominator") 
    else standard_form (r1*r2 ,i1*i2)

  let div (r1,i1) (r2,i2) = 
    match   (r2,i2) with
    |(r2,i2)-> if r2 = 0 then raise (Bad_rational "stupid denominator") else mul (r1,i1) (i2,r2)

  let add_inv (r,i) = (-r,i)

  let mul_inv (r,i)=
    match   (r,i) with
    |(r,i)-> if r = 0 then raise (Bad_rational "stupid denominator") else  (i,r)
            
  let to_string (r,i)=match standard_form (r,i) with
    | (r1,i1) -> (string_of_int r1)^"/"^(string_of_int i1)
                                        
  let to_float (r,i)= (float_of_int r)/.(float_of_int i)
  let from_int n = (n,1) 

  
end

module GaussianRationals : GaussianRationalField with type t = (int*int) * (int*int) =
struct
  type t = (int * int) * (int * int)
  exception Division_by_zero of string
  let zero = ((0,1),(0,1))
  let one = ((1,1),(0,1))
  let compare (r1,i1) (r2,i2) =
    if (Rationals.compare r1 r2 <> 0) then Rationals.compare r1 r2 else Rationals.compare i1 i2
        
  let to_string (r,i) = 
    match Rationals.standard_form i with 
    | (0,0) -> Rationals.to_string r 
    | _ -> match i with 
      |(i1,i2) -> if i1*i2>0 then Rationals.to_string r ^ "+" ^ Rationals.to_string i ^ "*I"
          else Rationals.to_string r ^ Rationals.to_string i ^ "*I"
           
  let from_rational r = (r,Rationals.zero)
  
  let denominator_zero r = 
    match r with
    | ((_,0),(_,_)) -> true 
    | ((_,_),(_,0)) -> true
    | _ -> false
      
  let numerator_zero r = 
    match r with
    | ((0,_),(_,_)) -> true 
    | ((_,_),(0,_)) -> true
    | _ -> false
      
  let add a b =
    if (denominator_zero a || denominator_zero b) then raise (Division_by_zero "stupid denominator")
    else match a,b with
      | ((x,y),(z,t)) -> (Rationals.standard_form (Rationals.add x z),Rationals.standard_form (Rationals.add y t))
                         
  let sub a b =
    if (denominator_zero a || denominator_zero b) then raise (Division_by_zero "stupid denominator")
    else match a,b with
      | ((x,y),(z,t)) -> (Rationals.standard_form (Rationals.sub x z),Rationals.standard_form (Rationals.sub y t)) 
                         
  let mul a b = 
    if (denominator_zero a) || (denominator_zero b) then raise (Division_by_zero "stupid denominator") else
      match a,b with
      | (x,y), (z,t) ->
          ((Rationals.standard_form (Rationals.sub (Rationals.mul x z) (Rationals.mul y t))),
           (Rationals.standard_form (Rationals.add (Rationals.mul z y) (Rationals.mul x t))))
        
                         
  let div x y =
    match x,y with 
    |((a, b), (c, d)) ,((e, f), (g, h)) -> (
        if (denominator_zero x) || (denominator_zero y) then raise (Division_by_zero "stupid denominator") else
          (Rationals.standard_form (Rationals.div (Rationals.add (Rationals.mul (a, b) (e, f)) (Rationals.mul (c, d) (g, h)))
                                      (Rationals.add (Rationals.mul (e, f) (e, f)) (Rationals.mul (g, h) (g, h))))), 
          Rationals.standard_form (Rationals.div (Rationals.sub (Rationals.mul (c, d) (e, f)) (Rationals.mul (a, b) (g, h)))
                                     (Rationals.add (Rationals.mul (e, f) (e, f)) (Rationals.mul (g, h) (g, h))))
      )
            
  let re a = 
    if denominator_zero a 
    then raise (Division_by_zero "stupid denominator")
    else match a with
      | (x,_) -> Rationals.standard_form x 
  
  let im a =
    if denominator_zero a 
    then raise (Division_by_zero "stupid denominator")
    else match a with
      | (_,x) -> Rationals.standard_form x 
                   
  let add_inv a = 
    if denominator_zero a 
    then raise (Division_by_zero "stupid denominator")
    else match a with
      | (x,y) -> (Rationals.standard_form (Rationals.add_inv x),Rationals.standard_form (Rationals.add_inv y))
                 
  let mul_inv x =
    match x with
    | (a,b),(c,d) -> (
        if denominator_zero x then raise (Division_by_zero "stupid denominator") 
        else
          (Rationals.standard_form (Rationals.div (a, b) (Rationals.add (Rationals.mul (a, b) (a, b)) (Rationals.mul (c, d) (c, d))))),
          (Rationals.standard_form (Rationals.div (-c, d) (Rationals.add (Rationals.mul (a, b) (a, b)) (Rationals.mul (c, d) (c, d)))))
      )
                 
  let conj a = 
    if denominator_zero a 
    then raise (Division_by_zero "stupid denominator")
    else match a with
      | (x,(y,z)) -> (Rationals.standard_form x,Rationals.standard_form (-y,z)) 
      
      

   
end












