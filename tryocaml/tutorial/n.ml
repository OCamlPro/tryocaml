
type value =
    Float of float
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nat of nativeint

type comparable =
  | Scalar of float
  | Alpha of string

type t = {
  mutable comparable : comparable;
  mutable value : value;
}

let t value =
  let comparable = match value with
      Int n -> Scalar (float_of_int n)
    | Nat n -> Scalar (Nativeint.to_float n)
    | Int32 n -> Scalar (Int32.to_float n)
    | Int64 n -> Scalar (Int64.to_float n)
    | Float f -> Scalar f
  in
  { comparable; value }

let int_of_int n = t (Int n)
let nativeint_of_nativeint n = t (Nat n)
let float_of_float f = t (Float f)
let int32_of_int32 i = t (Int32 i)
let int64_of_int64 i = t (Int64 i)

let float (x : t) =
  match x.value with
      Float x -> x
    | Int n -> float_of_int n
    | Nat n -> Nativeint.to_float n
    | Int32 n -> Int32.to_float n
    | Int64 n -> Int64.to_float n

let int64 (x : t) =
  match x.value with
      Float x -> Int64.of_float x
    | Int n -> Int64.of_int n
    | Nat n -> Int64.of_nativeint n
    | Int32 n -> Int64.of_int32 n
    | Int64 n -> n

let int32 (x : t) =
  match x.value with
      Float x -> Int32.of_float x
    | Int n -> Int32.of_int n
    | Nat n -> Nativeint.to_int32 n
    | Int64 n -> Int64.to_int32 n
    | Int32 n -> n

let int (x : t) =
  match x.value with
      Float x -> int_of_float x
    | Int n -> n
    | Nat n -> Nativeint.to_int n
    | Int32 n -> Int32.to_int n
    | Int64 n -> Int64.to_int n

  let nat (x : t) =
    match x.value with
        Float x -> Nativeint.of_float x
      | Int n -> Nativeint.of_int n
      | Nat n -> n
      | Int32 n -> Nativeint.of_int32 n
      | Int64 n -> Int64.to_nativeint n

  let (+) (x : t) (y : t) =
    let y = match x.value, y.value with
        Float x, _  -> Float (x +. float y)
      | _, Float y -> Float (y +. float x)
      | Int64 x, _ -> Int64 (Int64.add x (int64 y))
      | _, Int64 y -> Int64 (Int64.add y (int64 x))
      | Int32 x, _ -> Int32 (Int32.add x (int32 y))
      | _, Int32 y -> Int32 (Int32.add y (int32 x))
      | Nat x, _ -> Nat (Nativeint.add x (nat y))
      | _, Nat y -> Nat (Nativeint.add y (nat x))
      | Int x, Int y -> Int (x+y)
    in
    (t y : t)

  let ( * ) (x : t) (y : t) =
    let y = match x.value, y.value with
        Float x, _ -> Float (x *. float y)
      | _, Float y -> Float (y *. float x)
      | Int64 x, _ -> Int64 (Int64.mul x (int64 y))
      | _, Int64 y -> Int64 (Int64.mul y (int64 x))
      | Int32 x, _ -> Int32 (Int32.mul x (int32 y))
      | _, Int32 y -> Int32 (Int32.mul y (int32 x))
      | Nat x, _ -> Nat (Nativeint.mul x (nat y))
      | _, Nat y -> Nat (Nativeint.mul y (nat x))
      | Int x, Int y -> Int (x*y)
    in
    (t y : t)

  let (-) (x : t) (y : t) =
    let y = match x.value, y.value with
        Float x, _ -> Float (x -. float y)
      | _, Float y -> Float (float x -. y)
      | Int64 x, _ -> Int64 (Int64.sub x (int64 y))
      | _, Int64 y -> Int64 (Int64.sub (int64 x) y)
      | Int32 x, _ -> Int32 (Int32.sub x (int32 y))
      | _, Int32 y -> Int32 (Int32.sub (int32 x) y)
      | Nat x, _ -> Nat (Nativeint.add x (nat y))
      | _, Nat y -> Nat (Nativeint.add (nat x) y)
      | Int x, _ -> Int (x - int y)
    in
    (t y : t)

  let ( / ) (x : t) (y : t) =
    let y = match x.value , y.value with
        Float x, _ -> Float (x /. float y)
      | _, Float y -> Float (float x /. y)
      | Int64 x, _ -> Int64 (Int64.div x (int64 y))
      | _, Int64 y -> Int64 (Int64.div (int64 x) y)
      | Int32 x, _ -> Int32 (Int32.div x (int32 y))
      | _, Int32 y -> Int32 (Int32.div (int32 x) y)
      | Nat x, _ -> Nat (Nativeint.div x (nat y))
      | _, Nat y -> Nat (Nativeint.div (nat x) y)
      | Int x, _ -> Int (x / int y)
    in
    (t y : t)

  let (~-) (x : t ) =
    let y =
      match x.value with
          Float x -> Float (~-. x)
        | Int n -> Int (-n)
        | Int32 n -> Int32 (Int32.sub Int32.zero n)
        | Int64 n -> Int64 (Int64.sub Int64.zero n)
        | Nat n -> Nat (Nativeint.sub Nativeint.zero n)
    in
    t y

  let init () = ()

  let print ppf x =
    match x.value with
      | Int n -> Format.fprintf ppf "@[Int %d@]" n
      | Int32 n -> Format.fprintf ppf "@[Int32 %ld@]" n
      | Int64 n -> Format.fprintf ppf "@[Int64 %Ld@]" n
      | Nat n -> Format.fprintf ppf "@[Nat %nd@]" n
      | Float f -> Format.fprintf ppf "@[Float %f@]" f



