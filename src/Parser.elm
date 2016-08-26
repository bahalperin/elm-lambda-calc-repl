module Parser exposing
  ( parse
  )

import Combine exposing
  ( Parser, Context
  , map, mapError, andThen
  , string, regex
  , skip
  , succeed, fail
  , rec, app, chainl, primitive
  , or, choice
  , between, parens
  , many1
  )
import Combine.Char exposing
  ( char
  , upper, lower
  )
import Combine.Num exposing (int)
import List.Extra
import String
import Syntax exposing
  ( Expr(..)
  , Lit(..)
  , Binop(..)
  )


letter : Parser Char
letter =
  upper
    |> or lower


identifier : Parser String
identifier =
  regex "[_a-z][_a-zA-Z0-9]*"


whitespace : Parser String
whitespace =
  regex "[ \t\r\n]*"


betweenWhitespace : Parser a -> Parser a
betweenWhitespace parser =
  parser
    |> between whitespace whitespace


withError : String -> Parser a -> Parser a
withError error =
  mapError (\_ -> [ error ])


bool : Parser Expr
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


number : Parser Expr
number =
  int
    |> map LInt
    |> map Lit
    |> withError "integer literal"


variable : Parser Expr
variable =
  identifier
    |> map Var


lambda : Parser Expr
lambda =
  string "\\"
    |> (flip andThen)
      (\_ ->
        identifier
          |> betweenWhitespace
          |> many1
      )
    |> (flip andThen)
      (\args ->
        string "->"
          |> map (\_ -> args)
      )
    |> (flip andThen)
      (\args ->
        expr
          |> map (\body -> List.foldr Lam body args)
      )
    |> withError "lambda expression"


expr : Parser Expr
expr =
  rec <| \() ->
    term


term : Parser Expr
term =
  rec <| \() ->
    buildExpressionParser table aexp


infixOp : String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f =
  string x
    |> map (\_ -> f)


buildExpressionParser : List (List (Parser (Expr -> Expr -> Expr))) -> Parser Expr -> Parser Expr
buildExpressionParser table aexp =
  List.foldl buildExpressionParserHelp aexp table


buildExpressionParserHelp : List (Parser (Expr -> Expr -> Expr)) -> Parser Expr -> Parser Expr
buildExpressionParserHelp ops aexp =
  choice ops
    |> chainl aexp


table : List (List (Parser (Expr -> Expr -> Expr)))
table =
    [
      [ infixOp "*" (Op Mul)
      ]
    , [ infixOp "+" (Op Add)
      , infixOp "-" (Op Sub)
      ]
    , [ infixOp "==" (Op Eql)
      ]
    ]


aexp : Parser Expr
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
    |> (flip andThen)
      (\terms ->
        case (List.Extra.foldl1 App terms) of
          Just val ->
            succeed val
          Nothing ->
            fail [""]
      )


program : Parser (List Expr)
program =
  let
    all acc cx =
      if cx.input == ""
      then (Ok (List.reverse acc), cx)
      else
        case app expr cx of
          (Ok res', cx') ->
            all (res' :: acc) cx'

          (Err ms, cx') ->
            (Err ms, cx')
  in
    primitive <| all []


formatError : String -> List String -> Context -> String
formatError input ms cx =
  let
    lines = String.lines input
    lineCount = List.length lines
    (line, lineNumber, lineOffset, _) =
      List.foldl
            (\line (line', n, o, pos) ->
               if pos < 0
               then (line', n, o, pos)
               else (line, n + 1, pos, pos - 1 - String.length line'))
            ("", 0, 0, cx.position) lines

    separator = "|> "
    expectationSeparator = "\n  * "
    lineNumberOffset = floor (logBase 10 lineNumber) + 1
    separatorOffset = String.length separator
    padding = lineNumberOffset + separatorOffset + lineOffset + 1
  in
    "Parse error around line:\n\n"
      ++ (toString lineNumber) ++ separator ++ line ++ "\n"
      ++ String.padLeft padding ' ' "^"
      ++ "\nI expected one of the following:\n"
      ++ expectationSeparator
      ++ String.join expectationSeparator ms


parse : String -> Result String (List Expr)
parse s =
  case Combine.parse program s of
    (Ok e, _) ->
      Ok e

    (Err ms, cx) ->
      Err <| formatError s ms cx
