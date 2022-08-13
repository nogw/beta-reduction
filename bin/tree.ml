type term = 
  | Var of string 
  | Abs of string * term 
  | App of term * term
  [@@deriving show]

let rec string_of_t = function 
  | Var v -> v 
  | Abs (param, body) -> Printf.sprintf "[λ%s -> %s]" param (string_of_t body)
  | App (lambda, arg) -> Printf.sprintf "%s %s" (string_of_t lambda) (string_of_t arg)

let rec js_of_t = function
  | Var v -> v 
  | Abs (param, body) -> Printf.sprintf "%s => %s" param (js_of_t body)
  | App (lambda, arg) -> 
    match lambda with 
    | Abs _ -> Printf.sprintf "(%s)(%s)" (js_of_t lambda) (js_of_t arg)
    | _ -> Printf.sprintf "%s(%s)" (js_of_t lambda) (js_of_t arg)

let show = fun x -> string_of_t x |> print_endline