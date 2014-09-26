let t1 = true

let t2 = false

let i = 1

let e = []

let xs = 1 :: 2 :: 3 :: []

let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1)

let add = fun n1 -> fun n2 -> n1 + n2

let sub = fun n1 -> fun n2 -> n1 - n2

let compare = fun n1 -> fun n2 -> n1 < n2

let num = fact 5

let t4 = true && false

let l = (fun x -> x) :: []

let f = fun x -> match x with
    true -> false
    | false -> true

