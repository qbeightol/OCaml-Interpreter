type 'a stream = Stream of 'a * (unit -> 'a stream)

let id           = fun x -> x
let fork (f,g) x = f x, g x
let rec take n (Stream (h,t)) = if n=0 then [] else h::take (n-1) (t ())

let rec unfold (f:  'a -> 'b * 'a) (s: 'a) : 'b stream = 
    match f s with (hd, s') -> Stream (hd, fun () -> (unfold f s'))

let univ (f,g) x = (fun x -> unfold (fork x)) (f,g) x

let hd (Stream (h,_)) = h
let tl (Stream (_,t)) = t ()

let repeat (x: 'a) : 'a stream = univ (id, id) x

let map (f: 'a -> 'b) (s: 'a stream) : 'b stream = 
    let f' = (fun x -> f (hd x)) in
        match s with  Stream(h,t) -> univ (f', tl) s

let diag (s: 'a stream stream) : 'a stream = 
    univ ((fun s' -> hd (hd s')), (fun s' -> map tl (tl s'))) s

let suffixes (s: 'a stream) : 'a stream stream = univ (id, tl) s

let interleave (s: 'a stream) (s': 'a stream) : 'a stream =
    univ ((fun (s1,_) -> hd s1), (fun (s1,s2) -> s2, (tl s1))) (s, s')

let fibs = univ ((fun (e, _) -> e), (fun (e1, e2) -> (e1+e2, e1))) (0,1)


let one_to_n n = if (n mod 2 = 0) then 1. else -1.

(*computes the nth term--but not partial sum--of  the summation identity for
   pi provided on page 5 of the ps4 handout.*)
let nth_term n = 4. *. ((one_to_n n)/. (2. *. (float_of_int n) +. 1.))

let pi = 
    univ ((fun (s, n) -> s), (fun (s,n) -> (s +. nth_term n, succ n))) (4., 1)

let look (l: int list) : (int * int) list =
    let look (e: int) (counts: (int * int) list) : (int * int) list =
        (match counts with
        | [] -> (1,e)::[]
        | (hc, he)::t -> if (he = e) then (hc + 1, he)::t
                                else (1, e)::counts) in
    List.fold_right look l []

let say (l: (int * int) list ) : int list =
    let say ((c, e) : (int * int)) (old: int list) : int list = c::e::old in
    List.fold_right say l []

let look_and_say = univ (id, (fun x -> say (look x))) [1]
