module Eval
    exposing
        ( runEval
        )

import Dict exposing (Dict)
import Result
import Syntax
    exposing
        ( Expr(..)
        , Lit(..)
        , Binop(..)
        )


type Value
    = VInt Int
    | VBool Bool
    | VClosure String Expr Scope


type alias Scope =
    Dict String Value


eval : Scope -> Expr -> Result String Value
eval env expr =
    case expr of
        Lit (LInt x) ->
            Ok (VInt x)

        Lit (LBool x) ->
            Ok (VBool x)

        Var x ->
            case Dict.get x env of
                Just val ->
                    Ok (val)

                Nothing ->
                    Err ("error: variable is not in scope: " ++ x)

        Op op a b ->
            let
                result =
                    Result.map2 (,) (eval env a) (eval env b)
            in
                case result of
                    Ok ( VInt valA, VInt valB ) ->
                        Ok (binop op valA valB)

                    Ok _ ->
                        Err "Tried to evaluate operator on a non-Int value"

                    Err errA ->
                        Err errA

        Lam x body ->
            Ok (VClosure x body env)

        App a b ->
            let
                result =
                    Result.map2 (,) (eval env a) (eval env b)
            in
                case result of
                    Ok ( valA, valB ) ->
                        apply valA valB

                    Err error ->
                        Err error


binop : Binop -> Int -> Int -> Value
binop op a b =
    case op of
        Add ->
            VInt (a + b)

        Mul ->
            VInt (a * b)

        Sub ->
            VInt (a - b)

        Eql ->
            VBool (a == b)


extend : Scope -> String -> Value -> Scope
extend env v t =
    Dict.insert v t env


apply : Value -> Value -> Result String Value
apply closure ex =
    case closure of
        VClosure n e clo ->
            eval (extend clo n ex) e

        _ ->
            Err "Tried to apply closure"


emptyScope : Scope
emptyScope =
    Dict.empty


valToString : Value -> String
valToString val =
    case val of
        VInt int ->
            int |> toString

        VBool bool ->
            bool |> toString

        VClosure name expr scope ->
            "<<closure>>"


runEval : Expr -> String
runEval expr =
    case eval emptyScope expr of
        Ok val ->
            val |> valToString

        Err str ->
            str
