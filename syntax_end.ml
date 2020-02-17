(* ML interpreter / type reconstruction *)

type id = string


type binOp = Plus | Mult | Lt | And | Or


type exp =
  | Var of id (* Var "x" --> x *)
  | ILit of int (* ILit 3 --> 3 *)
  | BLit of bool (* BLit true --> true *) 
  | BinOp of binOp * exp * exp
  (* BinOp(Plus, ILit 4, Var "x") --> 4 + x *)
  | IfExp of exp * exp * exp
  (* IfExp(BinOp(Lt, Var "x", ILit 4), 
           ILit 3, 
           Var "x") --> 
     if x<4 then 3 else x *)
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | DFunExp of id * exp      
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp



type program = 
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp




type tyvar = int
      


type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty



let rec freevar_ty ty =

  match ty with

    TyInt -> MySet.empty

  | TyBool -> MySet.empty

  | TyVar(tyvar') -> MySet.singleton tyvar'

  | TyFun(a, b) -> MySet.union (freevar_ty a) (freevar_ty b)

(* 与えられた型中の型変数の集合を返す関数 *)



let var_list = ref []
 
     

let rec pp_ty_ =
  function

    TyInt -> print_string "int"

  | TyBool -> print_string "bool"

  | TyVar(tyvar') ->


      let var_id =

         let f tyvar =

      let rec val_count counter = function

          [] -> var_list := !var_list @ [tyvar]; counter

        | x :: rest -> if x = tyvar then counter else val_count (counter + 1) rest

      in val_count 0 !var_list

      in f in

     print_string ("'" ^ (String.make 1 (char_of_int (int_of_char 'a' + (var_id tyvar') mod 26 ) ) ) ^ if ((var_id tyvar')/26) = 0 then "" else (string_of_int((var_id tyvar')/26) )  )

  | TyFun(a, b) ->


     match (a, b) with

       (TyFun(c,d), TyFun(e,f)) ->

	 print_string "(";

	 pp_ty_ (TyFun(c,d));

	 print_string ")";

	 print_string " -> ";

	 pp_ty_ (TyFun(e,f))

     | (TyFun(c,d), _) ->

	 print_string "(";

	 pp_ty_ (TyFun(c,d));

	 print_string ")";

	 print_string " -> ";

	 pp_ty_ b

     | (_, TyFun(e,f)) ->

	 pp_ty_ a;

                 print_string " -> ";

	 pp_ty_ (TyFun(e,f));

     | _ ->

     pp_ty_ a;

     print_string " -> ";

     pp_ty_ b



let pp_ty x = pp_ty_ x;

var_list := []



let fresh_tyvar =

  let counter = ref 0 in

  let body () =

    let v = !counter in

      counter := v + 1; v

  in body

(* この関数は引数として()を渡すと（すなわちfresh_tyvar ()のように呼び出すと）新しい未使用の型変数を生成する. *)
