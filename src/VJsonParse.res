open VJsonTypes

let parseVJsonWithVariable = parseVariableString => {
  open // (variableFromString: string => result('v, string)) => {

  ReludeParse.Parser

  let lexeme: 'a. t<'a> => t<
    'a,
  > = // Run a parser and transparently consume all trailing whitespace.
  p => \"<*"(p, ws)

  // Parse the "null" keyword"
  let parseNull: t<vjson<'v>> = str("null") |> map(_ => Null) |> lexeme

  // Note: regex taken from https://rgxdb.com/r/1RSPF8MG, but the '$'
  // at the end has been omitted because it causes parse errors
  let floatRegex: Js.Re.t = %re(`/^([-+]?\\d*\\.?\\d+)(?:[eE]([-+]?\\d+))?/`)

  // Parse a number (float or int)
  let parseNumber: t<float> =
    \"<?>"(regex(floatRegex), "Not a valid number") |> map(Js.Float.fromString) |> lexeme

  // Parse a boolean.
  let parseBool: t<bool> =
    \"<|>"(str("true") |> map(_ => true), str("false") |> map(_ => false)) |> lexeme

  // Parse a string. Allows for escaped quotes.
  // NOTE: not to be confused with `parse`, which takes a raw string and parses
  // a whole AST -- this parses string literal syntax.
  let parseString: t<string> =
    betweenDoubleQuotes(
      many(\"<|>"(str("\\\"") |> map(_ => "\""), anyCharNotIn(list{"\""}))) |> map(l =>
        l->Belt.List.toArray->Js.Array2.joinWith("")
      ),
    ) |> lexeme

  // Parse a variable wrapped in a pair of doubled curly braces `{{ }}`. The
  // string of text between the curly braces is parsed by `parseVariable`.
  let parseVariable_ = \">>="(
    \"*>"(
      \"*>"(str("{{"), ws),
      manyUntil(str("}}"), \"<|>"(str("\\}}") |> map(_ => "}}"), anyChar)),
    ) |> map(l => l->Belt.List.toArray->Js.Array2.joinWith("")->Js.String.trim),
    rawVariableString =>
      switch parseVariableString(rawVariableString) {
      | Ok(variable) => pure(variable)
      | Error(message) => fail(message)
      },
  )

  // Parse an array of VJson.
  // Define these as lazy because of mutual recursion.
  let rec parseArray: Lazy.t<t<array<vjson<'v>>>> = lazy (
    betweenSquares(parseVjsonLazy->Lazy.force |> sepByOptEnd(str(",") |> lexeme))
    |> map(Belt.List.toArray)
    |> lexeme
  )

  and parseObject: Lazy.t<t<Js.Dict.t<vjson<'v>>>> = lazy {
    let parseKeyValuePair: t<(string, vjson<'v>)> = tuple2(
      \"<*"(parseString, str(":")) |> lexeme,
      parseVjsonLazy->Lazy.force,
    )
    betweenCurlies(parseKeyValuePair |> sepByOptEnd(str(",") |> lexeme))
    |> map(pairs => pairs->Belt.List.toArray->Js.Dict.fromArray)
    |> lexeme
  }

  and parseVjsonLazy: Lazy.t<t<vjson<'v>>> = lazy (
    \"<|>"(
      \"<|>"(
        \"<|>"(
          \"<|>"(parseNull, parseString |> map(s => String(s))),
          parseNumber |> map(n => Number(n)),
        ),
        parseBool |> map(b => Bool(b)),
      ),
      parseVariable_ |> map(v => Variable(v)),
    )
    |> orElseLazy(~fallback=() => parseArray->Lazy.force |> map(arr => Array(arr)))
    |> orElseLazy(~fallback=() => parseObject->Lazy.force |> map(d => Object(d->JsMap.fromDict)))
  )

  parseVjsonLazy->Lazy.force
}
