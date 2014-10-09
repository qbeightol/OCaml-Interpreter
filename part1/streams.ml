type 'a stream = Stream of 'a * (unit -> 'a stream)

let id           = fun x -> x
let fork (f,g) x = f x, g x
let comp f g x = f (g x)

let foldl1 f f_hd l =
  match l with
  | hd::tl -> List.fold_right f (f_hd hd) tl
  | _ -> failwith "empty list"

(*returns a list containing the first n elements of a string*)
let rec take n (Stream (h,t)) = if n=0 then [] else h::take (n-1) (t ())

(*takes in a generating function, [f] and a seed and produces a stream
  note: [f] takes in a seed and returns the next element in the stream, along
  with a new seed for generating the rest of the stream*)
let rec unfold (f:  'a -> 'b * 'a) (s: 'a) : 'b stream =
  let (hd, s') = f s in Stream (hd, fun () -> unfold f s')

(*univ behaves like unfold only it takes a tuple of generators, where the first
  generating function produces a new element using the current seed, and the
  second produces the next seed*)
let univ fs s = unfold (fork fs) s

let hd (Stream (h,_)) = h
let tl (Stream (_,t)) = t ()

let rec nth n s =
  if n < 0 then failwith "nth isn't defined for negative n"
  else if n = 0 then hd s
  else nth (n-1) (tl s)

(*creates a stream consisting only of x*)
let repeat x = univ (id, id) x

(*applies f (lazily) to each element of the stream*)
let map f s = univ (comp f hd, tl) s

(*returns the diagonal (as a stream) of a stream of streams; i.e. it returns
  a stream whose ith element is the ith element from ith stream in the input *)
let diag s = univ (comp hd hd, comp (map tl) tl) s

(*takes in a stream s0 s1 s2 s3 ... and returns
  s0  s1  s2  ...
  s1  s2  s3  ...
  s2  s3  s4  ...
  ... ... ... ... *)
let suffixes (s: 'a stream) : 'a stream stream = univ (id, tl) s

(*takes in a stream s0 s1 s2 ... and another stream t0 t1 t2 ... and returns
  s0 t0 s1 t1 s2 t2 ... *)
let interleave s t = univ (comp hd fst, fun (s,t) -> t, tl s) (s,t)

(*a stream of fibonacci numbers*)
let fibs = univ (fst, fun (e, e') -> e + e', e) (0,1)

(*calculates 1 to the nth power*)
let one_to_n n = if (n mod 2 = 0) then 1. else -1.

(*computes the nth term--but not partial sum--of  the summation identity for
   pi provided on page 5 of the ps4 handout.*)
let nth_term n = 4. *. ((one_to_n n)/. (2. *. (float_of_int n) +. 1.))

(*a stream whose nth element is nth partial sum of the formula for pi from the
  handout*)
let pi = univ (fst, fun (s,n) -> s +. nth_term n, succ n) (0.0, 0)


  let f (n, count, l) e = if e = n then (n, count + 1, l) else (e, 0, n::e::l)
  in let (_, _, lst) = foldl1 f (fun hd -> [0;hd] l in lst

let look_n_say (l: int list) : int list =
  let f (n::count::l) e = if e = n then n::(count + 1)::l else e::1::n::count::l
  in List.rev (foldl1 f (fun hd -> [hd;1]) l)

let look_and_say = univ (id, look_n_say) [1]
