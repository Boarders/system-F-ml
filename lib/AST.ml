open Base__List;;

module Ty = struct
  type prim_t = Bool
  type ty =
    | Prim of prim_t
    | Fun of ty * ty [@@deriving show]
end

type prim =
  | Bool of bool

type var = string

type raw_expr =
  | Var of var
  | Lambda of var * Ty.ty * raw_expr
  | App of raw_expr * raw_expr
  | Let of var * raw_expr * raw_expr
  | Prim of prim
  | IfThenElse of raw_expr * raw_expr * raw_expr

type index =
  | Ind of int

type level =
  | Lvl of int

type expr =
  | Var of index
  | Lambda of string * Ty.ty * expr
  | App of expr * expr
  | Let of var * expr * expr
  | Prim of prim
  | IfThenElse of expr * expr * expr

module Value = struct
  type closure =
    | Closure of env * expr
  and env = value list
  and spine = value list
  and neutral =
    | Neu_spine of spine
  and value =
    | V_lambda of closure
    | V_neu of neutral
    | V_prim of prim

  let add_spine (e : spine) (v : value) : spine = cons v e

  let add_env (e : env) (v : value) = cons v e
  let lookup_env (e : env) (ix : index) : value =
    match ix with
    | Ind i -> nth_exn e i
  let eval_prim (p : prim) : value = V_prim(p)
  let rec eval (env : env) (expr : expr) : value =
    match expr with
    | Var i  -> lookup_env env i
    | Lambda (_, _, body) -> V_lambda (Closure (env, body))
    | App (f, e) -> v_app (eval env f) (eval env e)
    | Let (_,bound, body) ->
        eval (add_env env (eval env bound)) body
    | Prim p -> eval_prim p
    | IfThenElse (bl, th, el) ->
        match (eval env bl) with
        | V_prim (Bool b) ->
           if b then (eval env th) else (eval env el)
        | _ -> failwith "eval (if_then_else): type error"
  and app_closure (clos : closure) (arg : value) : value =
    match clos with
    | Closure (env, body) -> eval (add_env env arg) body
  and v_app (fn : value) (arg : value) : value =
    match fn with
    | V_lambda cl -> app_closure cl arg
    | V_neu (Neu_spine sp) -> V_neu (Neu_spine (add_spine sp arg))
    | _ -> failwith "v_app: type error"
end
