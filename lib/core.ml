type aexp =
    Var of string
  | Int of int
  | Add of aexp * aexp
  | Mul of aexp * aexp
  [@@deriving show]

type bexp =
    Bool     of bool
  | LessThan of aexp * aexp
  [@@deriving show]

type cexp =
    Skip
  | Print  of string
  | Assign of string * aexp
  | Seq    of cexp * cexp
  | Cond   of bexp * cexp * cexp
  | While  of bexp * cexp
  [@@deriving show]

let lookup store key   = List.assoc key store 
let update store key v = (key, v) :: store 

let rec reduce_cexp store = function
| Skip                    -> (store, Skip)
| Print(n)                -> let ve = lookup store n in
                               Printf.printf "[print %s = %d]\n" n ve;
                               (store, Skip)
| Assign(n, v)            -> let ve = reduce_aexp store v in
                             let se = update store n ve in
                               (se, Skip)

| Seq(Skip, b)            -> (store, b)
| Seq(a, b)               -> let (se, ae) = reduce_cexp store a in
                               (se, Seq(ae, b))

| Cond(Bool(true), a, _)  -> (store, a)
| Cond(Bool(false), _, b) -> (store, b) 
| Cond(p, a, b)           -> let p_ = reduce_bexp store p in
                               (store, Cond(p_, a, b))

| While(p, a)             -> (store, Cond (p, Seq (a, While (p, a)), Skip))

and reduce_bexp store = function
| Bool(_) as b   -> b
| LessThan(a, b) -> let ae = reduce_aexp store a in
                    let be = reduce_aexp store b in
                      Bool(ae < be)

and reduce_aexp store = function
| Var(s)    -> lookup store s
| Int(i)    -> i
| Add(a, b) -> let ae = reduce_aexp store a in
               let be = reduce_aexp store b in
                 ae + be
| Mul(a, b) -> let ae = reduce_aexp store a in
               let be = reduce_aexp store b in
                 ae * be
