let variableRegex = [%re {|/[a-zA-Z_][a-zA-Z0-9_]*/|}];
module VariableName =
  Opaque.String.Make(
    (
      Opaque.String.Validation.MatchRegex({
        let regex = variableRegex;
      })
    ),
    {},
  );

type ast =
  | Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Variable(VariableName.t)
  | Array(array(ast))
  | Object(Js.Dict.t(ast));

let rec toJson =
  Json.Encode.(
    fun
    | Null => null
    | Bool(b) => b |> bool
    | Number(n) => n |> float
    | String(s) => s |> string
    | Array(arr) => arr |> array(toJson)
    | Object(d) => d |> dict(toJson)
    | Variable(var) => object_([("__var__", var |> VariableName.toJson)])
  );

module Parser = {
  include ReludeParse.Parser;
  let lexeme = p => p <* ws;
  let parseNull: t(ast) = str("null") |> map(_ => Null) |> lexeme;
  // Note: regex taken from https://rgxdb.com/r/1RSPF8MG, but the '$'
  // at the end has been omitted because it causes parse errors
  let floatRegex: Js.Re.t = [%re {|/^([-+]?\d*\.?\d+)/|}];

  let parseNumber: t(float) =
    regex(floatRegex)
    <?> "Not a valid number"
    |> map(Js.Float.fromString)
    |> lexeme;

  let parseBool: t(bool) =
    str("true")
    |> map(_ => true)
    <|> (str("false") |> map(_ => false))
    |> lexeme;

  let parseString: t(string) =
    betweenDoubleQuotes(
      many(str("\\\"") |> map(_ => "\"") <|> anyCharNotIn(["\""]))
      |> map(l => l->Belt.List.toArray->Js.Array2.joinWith("")),
    )
    |> lexeme;

  let parseVariableName: t(VariableName.t) =
    regex(variableRegex)
    <?> "Valid variable names contain only letters, numbers and/or underscores, and must begin with a letter or underscore"
    |> map(VariableName.fromString);

  let parseVariable: t(ast) =
    parseVariableName
    |> between(str("{{"), str("}}"))
    |> map(vn => Variable(vn));

  let rec parseArray: Lazy.t(t(array(ast))) =
    lazy(
      betweenSquares(
        parseAstLazy->Lazy.force |> sepByOptEnd(str(",") |> lexeme),
      )
      |> map(Belt.List.toArray)
      |> lexeme
    )

  and parseObject: Lazy.t(t(Js.Dict.t(ast))) =
    lazy({
      let parseKeyValuePair: t((string, ast)) =
        tuple2(parseString <* str(":") |> lexeme, parseAstLazy->Lazy.force);
      betweenCurlies(parseKeyValuePair |> sepByOptEnd(str(",") |> lexeme))
      |> map(pairs => pairs->Belt.List.toArray->Js.Dict.fromArray);
    })

  and parseAstLazy: Lazy.t(t(ast)) =
    lazy(
      parseNull
      <|> (parseString |> map(s => String(s)))
      <|> (parseNumber |> map(n => Number(n)))
      <|> (parseBool |> map(b => Bool(b)))
      <|> parseVariable
      |> orElseLazy(~fallback=() =>
           parseArray->Lazy.force |> map(arr => Array(arr))
         )
      |> orElseLazy(~fallback=() =>
           parseObject->Lazy.force |> map(d => Object(d))
         )
    );

  let parseAst = parseAstLazy->Lazy.force;

  let parse: string => result(ast, ParseError.t) =
    input => parseAst <* eof |> runParser(input);
};

/* Js.log( */
/*   Parser.(parseAst |> runParser("[{{a1}},{{a2}}]"))->Belt.Result.map(toJson), */
/* ); */
