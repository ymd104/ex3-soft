open Syntax
exception Error of string
let err s = raise (Error s)
(* Type Environment *)
type tyenv = ty Environment.t


type subst = (tyvar * ty) list


let rec sub_type (a, b) ty' =
  match ty' with
    TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar(tyvar') -> if tyvar' = a then b else TyVar(tyvar')
  | TyFun(x,y) -> TyFun ((sub_type (a, b) x), (sub_type (a, b) y))  


     

let rec subst_type subst' ty' =
  match subst' with
    [] -> ty'
  | (a, b)::rest -> subst_type rest (sub_type (a, b) ty')
(* 型代入subst'を型ty'に作用させたものを返す *)






(* eqs_of_subst : subst -> (ty * ty) list
型代入を型の等式集合に変換*)
let rec eqs_of_subst s =
  match s with
    [] -> []
  | (a,b)::rest -> (TyVar (a),b)::(eqs_of_subst rest)
  
(* subst_eqs: subst -> (ty * ty) list -> (ty * ty) list
型の等式集合に型代入を適用*)
let rec subst_eqs s eqs =
  match eqs with
    [] -> []
  | (a,b)::rest -> ((subst_type s a),(subst_type s b)) :: (subst_eqs s rest)


let rec unify l =
  match l with
    [] -> []
  | (TyInt,TyInt)::rest -> unify rest
  | (TyBool,TyBool)::rest -> unify rest
  | (TyVar a,TyVar b)::rest -> if a = b then unify rest else (a, TyVar b) :: (unify (subst_eqs [(a, TyVar b)] rest)) 
  | (TyFun(a,b),TyFun(c,d))::rest -> unify ((a,c)::(b,d)::rest)
  | (TyVar(a),b)::rest -> if MySet.member a (Syntax.freevar_ty b) then  err ("Type error")
                          else (a, b)::(unify (subst_eqs [(a,b)] rest))
  | (b,TyVar(a))::rest -> if MySet.member a (Syntax.freevar_ty b) then  err ("Type error")
                          else (a, b)::(unify (subst_eqs [(a,b)] rest))
  | _ -> err ("Type error")



  
let ty_prim op ty1 ty2 = match op with
Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)

| Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)

| Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  
| _ -> err "Not Implemented!"


(*
let rec ty_exp tyenv = function
Var x ->
(try Environment.lookup x tyenv with
Environment.Not_bound -> err ("variable not bound: " ^ x))
| ILit _ -> TyInt
| BLit _ -> TyBool
| BinOp (op, exp1, exp2) ->
let tyarg1 = ty_exp tyenv exp1 in
let tyarg2 = ty_exp tyenv exp2 in
ty_prim op tyarg1 tyarg2
| IfExp (exp1, exp2, exp3) ->
   if ty_exp tyenv exp1 = TyBool && ty_exp tyenv exp2 = ty_exp tyenv exp3 then ty_exp tyenv exp2
   else err("Type error")
| LetExp (id, exp1, exp2) ->
   ty_exp (Environment.extend id (ty_exp tyenv exp1) tyenv) exp2
| _ -> err ("Not Implemented!")
let ty_decl tyenv = function
Exp e -> ty_exp tyenv e
| _ -> err ("Not Implemented!")
*)


   
let rec ty_exp tyenv = function
    Var x ->
      (try ([], Environment.lookup x tyenv) with
        Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
			 
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = (ty1,TyBool) :: (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty2, ty3)] in
      let s4 = unify eqs in (s4, subst_type s4 ty2)
			 
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs = (eqs_of_subst s1) @ [(domty, ty1)] @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)

  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id domty tyenv) exp in
          (s, TyFun (subst_type s domty, ranty))
	
  | AppExp (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs = (eqs_of_subst s1) @ [(ty1, TyFun(ty2, domty))] @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 domty)
			 
  | _ -> err "Not Implemented!"


let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | Decl (_, e) -> ty_exp tyenv e
  | _ -> err "Not Implemented!"

