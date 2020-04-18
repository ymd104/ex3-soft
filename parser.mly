%{
open Syntax
%}


%token LPAREN RPAREN SEMISEMI

%token PLUS MULT LT AND OR

%token IF THEN ELSE TRUE FALSE

%token LET IN EQ

%token RARROW FUN DFUN

%token REC


%token <int> INTV

%token <Syntax.id> ID


%start toplevel

%type <Syntax.program> toplevel

%%



toplevel :

    e=Expr SEMISEMI { Exp e }

  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }

  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e) }



Expr :

    e=IfExpr { e }

  | e=LetExpr { e }

  | e=OrExpr { e }

  | e=AndExpr { e }
 
  | e=LTExpr { e }

  | e=FunExpr { e }

  | e=DFunExpr { e }
 
  | e=LetRecExpr { e }



OrExpr :

    TRUE OR Expr { BLit true }

  | TRUE OR ID { BLit true }

  | l=Expr OR r=Expr { BinOp (Or, l, r) }



AndExpr :

    l=FALSE AND Expr { BLit false }
  
  | l=FALSE AND ID { BLit false }

  | l=Expr AND r=Expr { BinOp (And, l, r) }
  

 
LTExpr :
 
  l=PExpr LT r=PExpr { BinOp (Lt, l, r) }

  | e=PExpr { e }



PExpr :

  l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }

  | e=MExpr { e }



MExpr :
 
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }

  | e=AppExpr { e }



AppExpr :

    e1=AppExpr e2=AExpr { AppExp (e1, e2) }

  | e=AExpr { e }



AExpr :
    i=INTV { ILit i }

  | TRUE   { BLit true }

  | FALSE  { BLit false }

  | i=ID   { Var i }

  | LPAREN e=Expr RPAREN { e }



IfExpr :

  IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }



LetExpr :
  LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }






FunExpr :
  FUN x=ID RARROW e=Expr { FunExp (x, e) }



DFunExpr :
  DFUN x=ID RARROW e=Expr { DFunExp (x, e) }   
   


LetRecExpr  :
  LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp(x1, x2, e1, e2) }
