
type t

val int_of_int : int -> t
val nativeint_of_nativeint : nativeint -> t
val float_of_float : float -> t
val int32_of_int32 : int32 -> t
val int64_of_int64 : int64 -> t

val int : t -> int
val int32 : t -> int32
val int64 : t -> int64
val nat : t -> nativeint
val float : t -> float

val ( + ) : t  -> t -> t
val ( - ) : t  -> t -> t
val ( ~- ) : t  -> t
val ( * ) : t  -> t -> t
val ( / ) : t  -> t -> t

val init : unit -> unit

val print : Format.formatter -> t -> unit
