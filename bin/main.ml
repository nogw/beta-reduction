open Tree

let rec abstraction l body =
  match l with
  | [] -> body
  | h :: [] -> Abs (h, body)
  | h :: t -> Abs (h, abstraction t body)

let rec application = function
  | [ x; y ] -> App (x, y)
  | x :: y :: t -> application (App (x, y) :: t)
  | _ -> failwith "maximum number of arguments"

let rec alpha a b = function
  | Var v -> if v = a then b else Var v
  | App (e1, e2) -> App (alpha a b e1, alpha a b e2)
  | Abs (v, body) -> if v = a then Abs (v, body) else Abs (v, alpha a b body)

let rec reduce = function
  | Var v -> Var v
  | Abs (v, body) -> Abs (v, body)
  | App (e1, e2) -> (
      match reduce e1 with
      | Abs (v, body) -> alpha v (reduce e2) body |> reduce
      | _ -> failwith "irreducible")

module Bool = struct
  let true_ = abstraction [ "x"; "y" ] (Var "x")
  let false_ = abstraction [ "x"; "y" ] (Var "y")
  let if_ = abstraction [ "c"; "t"; "f" ] (App (App (Var "c", Var "f"), Var "f"))
  let and_ = abstraction [ "a"; "b" ] (application [ Var "a"; Var "b"; false_ ])
  let or_ = abstraction [ "a"; "b" ] (application [ Var "a"; true_; Var "b" ])
  let not_ = abstraction [ "a" ] (application [ Var "a"; false_; true_ ])
end

module Number = struct
  let zero = abstraction [ "f"; "x" ] (Var "x")
  let succ = abstraction [ "n"; "f"; "x" ] (application [ Var "f"; (application [ Var "n"; Var "f"; Var "x" ]) ])
  let add = abstraction [ "m"; "n" ] (application [ Var "m"; succ; Var "n" ])
  let mult = abstraction [ "m"; "n" ] (application [ Var "m"; (abstraction [ "m" ] (application [ Var "n"; succ; Var "m" ])); zero ])
  let pred = abstraction [ "n"; "f"; "x" ] (application [ Var "n"; abstraction [ "g"; "h" ] (App (Var "h", App (Var "g", Var "f"))); Abs ("u", Var "x"); Abs ("u", Var "u"); ])
  let iszero = abstraction [ "n" ] (application [ Var "n"; Abs ("x", Bool.false_); Bool.true_ ])
end

module List = struct
  let pair = abstraction [ "x"; "y"; "z" ] (application [ Var "z"; Var "x"; Var "y" ])
  let first = abstraction [ "p" ] (application [ Var "p"; Bool.true_ ])
  let second = abstraction [ "p" ] (application [ Var "p"; Bool.false_ ])
  let nil = application [ pair; Bool.true_; Bool.true_ ]
  let isnil = fun p -> application [ first; p ]
  let cons = abstraction [ "a"; "b" ] (application [ pair; Bool.false_; application [ pair; Var "a"; Var "b" ] ])
  let head = abstraction [ "p" ] (application [ first; application [ second; Var "p" ] ])
  let tail = abstraction [ "p" ] (application [ second; application [ second; Var "p" ] ])
end

module Church = struct
  let rec int_of_church_encoding n =
    if Bool.true_ = (application [ Number.iszero; n ] |> reduce) then 0
    else 1 + int_of_church_encoding (application [ Number.pred; n ])
  
  let church_encoding_of_int n =
    let rec inner = function
      | 0 -> Var "x"
      | n ->
        if n < 0 then failwith "negative number"
        else App (Var "f", inner (n - 1))
    in abstraction [ "f"; "x" ] (inner n)
end

let z_combinator = 
  let z = Abs ("x", App (Var "f", (Abs ("v", application [ Var "x"; Var "x"; Var "v" ])))) 
  in Abs ("f", App (z, z))

let fact =
  let aux = abstraction [ "fact"; "n" ] (application [ application [ Number.iszero; (Var "n") ]; Abs ("_", application [ Number.succ; Number.zero ]); Abs ("_", application [ Number.mult; (Var "n") ; (App (Var "fact", application [ Number.pred; (Var "n") ])) ]); Abs ("x", Var "x"); ])
  in application [ z_combinator; aux ]

let () =
  application [fact; Church.church_encoding_of_int 5]
  |> reduce
  |> Church.int_of_church_encoding
  |> string_of_int
  |> print_endline;