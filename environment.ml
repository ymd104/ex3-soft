type 'a t = (Syntax.id * 'a) list

exception Not_bound

let empty = []
let extend x v env = (x,v)::env

let rec lookup x env = 
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest

let rec fold_right f env a = 
  match env with
      [] -> a
    | (_, v)::rest -> f v (fold_right f rest a)

let append env1 env2 = env1 @ env2



let rec bottomCut (env: 'a list) n =
  if n = 0 then env else bottomCut (List.rev (List.tl (List.rev env))) (n-1)
 


let merge env1 env2 =
  bottomCut env1 5 @ env2
  
