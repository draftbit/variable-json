open VJsonTypes

let parseVJsonWithVariable = parseVariableString => {
  open ReludeParse.Parser
  open Operators
  open Regexes

  let lexeme: 'a. t<'a> => t<
    'a,
  > = // Run a parser and transparently consume all trailing whitespace.
  p => left(p, ws)

  // Parse the "null" keyword"
  let parseNull: t<vjson<'v>> = str("null") |> map(_ => Null) |> lexeme

  // Parse a number (float or int)
  let parseNumber: t<float> =
    maybe(regex(floatRegex), "Not a valid number") |> map(Js.Float.fromString) |> lexeme

  // Parse a boolean.
  let parseBool: t<bool> =
    or_(str("true") |> map(_ => true), str("false") |> map(_ => false)) |> lexeme

  // Parse a string. Allows for escaped quotes.
  // NOTE: not to be confused with `parse`, which takes a raw string and parses
  // a whole AST -- this parses string literal syntax.
  let parseString: t<string> =
    regex(inQuotesRegex)
    |> map(match => {
      match
      ->Js.String2.replaceByRe(nonEscapedQuoteRegex, `$1`) // First group of the regex is characters before the quote that should be kept
      ->Js.String2.replaceByRe(escapedQuoteRegex, "\"")
    })
    |> lexeme

  // Parse a variable wrapped in a pair of doubled curly braces `{{ }}`. The
  // string of text between the curly braces is parsed by `parseVariable`.
  let parseVariable_ = flatMap(
    right(
      right(str("{{"), ws),
      manyUntil(str("}}"), or_(str("\\}}") |> map(_ => "}}"), anyChar)),
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
      left(parseString, str(":")) |> lexeme,
      parseVjsonLazy->Lazy.force,
    )
    betweenCurlies(parseKeyValuePair |> sepByOptEnd(str(",") |> lexeme))
    |> map(pairs => pairs->Belt.List.toArray->Js.Dict.fromArray)
    |> lexeme
  }

  and parseVjsonLazy: Lazy.t<t<vjson<'v>>> = lazy (
    parseNull
    ->or_(parseNumber |> map(n => Number(n)))
    ->or_(parseString |> map(s => String(s)))
    ->or_(parseBool |> map(b => Bool(b)))
    ->or_(parseVariable_ |> map(v => Variable(v)))
    |> orElseLazy(~fallback=() => parseArray->Lazy.force |> map(arr => Array(arr)))
    |> orElseLazy(~fallback=() => parseObject->Lazy.force |> map(d => Object(d->JsMap.fromDict)))
  )

  parseVjsonLazy->Lazy.force
}
