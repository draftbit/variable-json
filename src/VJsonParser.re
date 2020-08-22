open VJson;
include ReludeParse.Parser;

// Run a parser and transparently consume all trailing whitespace.
let lexeme: 'a. t('a) => t('a) = p => p <* ws;

// Parse the "null" keyword"
let parseNull: t(vjson) = str("null") |> map(_ => Null) |> lexeme;

// Note: regex taken from https://rgxdb.com/r/1RSPF8MG, but the '$'
// at the end has been omitted because it causes parse errors
let floatRegex: Js.Re.t = [%re {|/^([-+]?\d*\.?\d+)(?:[eE]([-+]?\d+))?/|}];

// Parse a number (float or int)
let parseNumber: t(float) =
  regex(floatRegex)
  <?> "Not a valid number"
  |> map(Js.Float.fromString)
  |> lexeme;

// Parse a boolean.
let parseBool: t(bool) =
  str("true")
  |> map(_ => true)
  <|> (str("false") |> map(_ => false))
  |> lexeme;

// Parse a string. Allows for escaped quotes.
// NOTE: not to be confused with `parse`, which takes a raw string and parses
// a whole AST -- this parses string literal syntax.
let parseString: t(string) =
  betweenDoubleQuotes(
    many(str("\\\"") |> map(_ => "\"") <|> anyCharNotIn(["\""]))
    |> map(l => l->Belt.List.toArray->Js.Array2.joinWith("")),
  )
  |> lexeme;

// Parse a variable.
let parseVariable: t(vjson) =
  regex(variableRegex)
  <?> "Valid variable names contain only letters, numbers and/or underscores, and must begin with a letter or underscore"
  |> between(str("{{"), str("}}"))
  |> map(vn => Variable(VariableName.fromString(vn)));

// Parse an array of VJson.
// Define these as lazy because of mutual recursion.
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
      tuple2(parseString <* str(":") |> lexeme, parseVjsonLazy->Lazy.force);
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

// Parse a string into a VJson. Prefer to `parseExn` which can raise an exception.
let parse: string => result(vjson, ReludeParse.Parser.ParseError.t) =
  input => ws *> parseVjson <* eof |> runParser(input);

// Parse a string
let parseExn: string => vjson = input => input->parse->Belt.Result.getExn;
