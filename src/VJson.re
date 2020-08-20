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

type vjson =
  | Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Variable(VariableName.t)
  | Array(array(vjson))
  | Object(Js.Dict.t(vjson));

// Shorthand
type t = vjson;

// Traverse a JSON structure with a function
let rec reduce: 'a. (vjson, 'a, ('a, vjson) => 'a) => 'a =
  (vjson, result, f) => {
    let newResult = f(result, vjson);
    switch (vjson) {
    | Bool(_)
    | Null
    | String(_)
    | Number(_)
    | Variable(_) => newResult
    | Array(arr) =>
      arr->Belt.Array.reduce(newResult, (r, j) => j->reduce(r, f))
    | Object(obj) =>
      obj
      ->Js.Dict.values
      ->Belt.Array.reduce(newResult, (r, j) => j->reduce(r, f))
    };
  };

let rec toJson = variableToJson =>
  Json.Encode.(
    fun
    | Null => null
    | Bool(b) => b |> bool
    | Number(n) => n |> float
    | String(s) => s |> string
    | Array(arr) => arr |> array(toJson(variableToJson))
    | Object(d) => d |> dict(toJson(variableToJson))
    | Variable(var) => var |> variableToJson
  );

let findVariables: vjson => JsSet.t(VariableName.t) =
  root =>
    root->reduce(JsSet.empty(), (vars, v) =>
      switch (v) {
      | Variable(vn) => vars->JsSet.addMut(vn)
      | _ => vars
      }
    );

module Parser = {
  include ReludeParse.Parser;
  let lexeme = p => p <* ws;
  let parseNull: t(vjson) = str("null") |> map(_ => Null) |> lexeme;
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

  let parseVariable: t(vjson) =
    parseVariableName
    |> between(str("{{"), str("}}"))
    |> map(vn => Variable(vn));

  let rec parseArray: Lazy.t(t(array(vjson))) =
    lazy(
      betweenSquares(
        parseVjsonLazy->Lazy.force |> sepByOptEnd(str(",") |> lexeme),
      )
      |> map(Belt.List.toArray)
      |> lexeme
    )

  and parseObject: Lazy.t(t(Js.Dict.t(vjson))) =
    lazy({
      let parseKeyValuePair: t((string, vjson)) =
        tuple2(
          parseString <* str(":") |> lexeme,
          parseVjsonLazy->Lazy.force,
        );
      betweenCurlies(parseKeyValuePair |> sepByOptEnd(str(",") |> lexeme))
      |> map(pairs => pairs->Belt.List.toArray->Js.Dict.fromArray);
    })

  and parseVjsonLazy: Lazy.t(t(vjson)) =
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

  let parseVjson = parseVjsonLazy->Lazy.force;
};

let parse: string => result(vjson, ReludeParse.Parser.ParseError.t) =
  input => Parser.(parseVjson <* eof |> runParser(input));

let parseExn: string => vjson = input => input->parse->Belt.Result.getExn;
