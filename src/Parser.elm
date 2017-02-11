module Parser
    exposing
        ( parse
        )

import Combine
    exposing
        ( Parser
        , ParseContext
        , map
        , mapError
        , andThen
        , string
        , regex
        , skip
        , succeed
        , fail
        , lazy
        , app
        , chainl
        , primitive
        , or
        , choice
        , between
        , parens
        , many1
        , manyTill
        , end
        )
import Combine.Char
    exposing
        ( char
        , upper
        , lower
        )
import Combine.Num exposing (int)
import List.Extra
import Syntax
    exposing
        ( Expr(..)
        , Lit(..)
        , Binop(..)
        )


letter : Parser s Char
letter =
    upper
        |> or lower


identifier : Parser s String
identifier =
    regex "[_a-z][_a-zA-Z0-9]*"


whitespace : Parser s String
whitespace =
    regex "[ \t\x0D\n]*"


betweenWhitespace : Parser s a -> Parser s a
betweenWhitespace parser =
    parser
        |> between whitespace whitespace


withError : String -> Parser s a -> Parser s a
withError error =
    mapError (\_ -> [ error ])


bool : Parser s Expr
bool =
    let
        boolLiteral =
            choice
                [ string "True"
                    |> map (\_ -> True)
                , string "False"
                    |> map (\_ -> False)
                ]
    in
        boolLiteral
            |> map LBool
            |> map Lit
            |> withError "boolean literal"


number : Parser s Expr
number =
    int
        |> map LInt
        |> map Lit
        |> withError "integer literal"


variable : Parser s Expr
variable =
    identifier
        |> map Var


lambda : Parser s Expr
lambda =
    string "\\"
        |> andThen
            (\_ ->
                identifier
                    |> betweenWhitespace
                    |> many1
            )
        |> andThen
            (\args ->
                string "->"
                    |> map (\_ -> args)
            )
        |> andThen
            (\args ->
                expr
                    |> map (\body -> List.foldr Lam body args)
            )
        |> withError "lambda expression"


expr : Parser s Expr
expr =
    lazy <|
        \() ->
            term


term : Parser s Expr
term =
    lazy <|
        \() ->
            buildExpressionParser table aexp


infixOp : String -> (a -> a -> a) -> Parser s (a -> a -> a)
infixOp x f =
    string x
        |> map (\_ -> f)


buildExpressionParser : List (List (Parser s (Expr -> Expr -> Expr))) -> Parser s Expr -> Parser s Expr
buildExpressionParser table aexp =
    List.foldl buildExpressionParserHelp aexp table


buildExpressionParserHelp : List (Parser s (Expr -> Expr -> Expr)) -> Parser s Expr -> Parser s Expr
buildExpressionParserHelp ops aexp =
    choice ops
        |> (\blah -> chainl blah aexp)


table : List (List (Parser s (Expr -> Expr -> Expr)))
table =
    [ [ infixOp "*" (Op Mul)
      ]
    , [ infixOp "+" (Op Add)
      , infixOp "-" (Op Sub)
      ]
    , [ infixOp "==" (Op Eql)
      ]
    ]


aexp : Parser s Expr
aexp =
    choice
        [ parens expr
        , number
        , bool
        , variable
        , lambda
        ]
        |> betweenWhitespace
        |> many1
        |> andThen
            (\terms ->
                case (List.Extra.foldl1 App terms) of
                    Just val ->
                        succeed val

                    Nothing ->
                        fail ""
            )


program : Parser s (List Expr)
program =
    manyTill aexp end


parse : String -> Result String (List Expr)
parse s =
    case Combine.parse program s of
        Ok ( a, b, e ) ->
            Ok e

        Err _ ->
            Err <| "Fail"
