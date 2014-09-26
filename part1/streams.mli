(** The [Streams] module is used to manipulate infinite sequences. *)

(** The ['a stream] type is used to represent infinite sequences with
    elements of type ['a]. The [+] in the type signature means that
    this type is {{:
    http://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)}
    covariant} in ['a]. For practical purposes, this lets the interpreter
    perform more advanced type inference. *)
type +'a stream

(** [fork] is a combinator for executing two functions on the same
    input and returning the results. More precisely,
    {[fork (f,g) x = (f x, g x)]} *)
val fork : ('a -> 'b) * ('a -> 'c) -> 'a -> 'b * 'c

(** [take n s] returns the first [n] elements of the stream [s] in a
    list. *)
val take : int -> 'a stream -> 'a list

(** [unfold f s] takes the seed [s] and a function [f] that produces a
    tuple containing two values: {ol {li a value to be placed into the
    stream, and} {li the next seed.}} The function [f] is applied
    successively to generate all of the values of the stream. *)
val unfold : ('a -> 'b * 'a) -> 'a -> 'b stream

(** The [univ] function is the composition of [unfold] and
    [fork]. Consequently, [univ (f,g) s] specifies a function [f] that
    generates the next element of the stream and [g] that generates
    the next seed. The function [univ] has a {{:
    http://en.wikipedia.org/wiki/Universal_property} universal
    property}. *)
val univ : ('a -> 'b) * ('a -> 'a) -> 'a -> 'b stream

(** [hd] returns the first element of a stream. *)
val hd : 'a stream -> 'a

(** [tl] returns the stream without its first element *)
val tl : 'a stream -> 'a stream

(** [repeat x] returns a stream that has every element equal to
    [x]. *)
val repeat : 'a -> 'a stream

(** [map f s] creates a new stream by applying [f] to each element of
    [s]. Returns in O(1) time. *)
val map : ('a -> 'b) -> 'a stream -> 'b stream

(** [diag ss] takes a stream of streams and returns a stream of the
    diagonal elements. That is, the [n]th element of the output stream is
    the [n]th element of the [n]th stream in the input. *)
val diag : 'a stream stream -> 'a stream

(** [suffixes s] returns a stream whose [n]th element is the substream
    of [s] starting at the [n]th element of [s]. *)
val suffixes : 'a stream -> 'a stream stream

(** [interleave s t] returns a stream with the elements of [s] and [t]
    in alternating order. *)
val interleave : 'a stream -> 'a stream -> 'a stream

(** [fibs] is a stream whose [n]th element is the [n]th fibonacci
    number. *)
val fibs : int stream

(** [pi] is a stream that contains a sequence of approximations that
    converge to the real number pi. The elements of this stream are
    the partial sums of {{:
    http://en.wikipedia.org/wiki/Approximations_of_π#Gregory.E2.80.93Leibniz_series}
    this series}. *)
val pi : float stream

(** [look_and_say] is a stream whose [n]th element is the [n]th number
    in the {{: http://en.wikipedia.org/wiki/Look-and-say_sequence}
    look-and-say sequence}. *)
val look_and_say : int list stream
