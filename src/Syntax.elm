module Syntax
    exposing
        ( Expr(..)
        , Lit(..)
        , Binop(..)
        )


type alias Name =
    String


type Expr
    = Lit Lit
    | Var Name
    | App Expr Expr
    | Lam Name Expr
    | Op Binop Expr Expr


type Lit
    = LInt Int
    | LBool Bool


type Binop
    = Add
    | Sub
    | Mul
    | Eql
