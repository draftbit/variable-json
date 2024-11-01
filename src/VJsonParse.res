open VJsonTypes
open StringParser

// NOTE: these parsers only return a `vjson` with an internal type of `string`.
// In order to use another variable type, use VJson.map and convert the variables
// from strings.

let lit = s => literal(s)->lexeme

let parseNull: parser<vjson<string>> = lit("null")->map(_ => Null)

let parseBool: parser<vjson<string>> = lit("true")->or_(lit("false"))->map(s => Bool(s === "true"))

let parseNumber: parser<vjson<string>> =
  regex(Regexes.number, "a number")->map(s => Number(s->Js.Float.fromString))->lexeme

let parseString: parser<string> =
  regex(Regexes.string, ~index=1, "a string literal")
  ->map(str => str->Js.String2.replaceByRe(Regexes.escapedQuote, "\""))
  ->lexeme

let parseVariable: Js.Re.t => parser<vjson<string>> = variableRegex =>
  lit("{{")
  ->right(regex(variableRegex, "a variable")->lexeme)
  ->left(lit("}}"))
  ->map(v => Variable(v))

let rec parseTerm: Js.Re.t => parser<vjson<string>> = (variableRegex, s) => {
  let parseArray =
    lit("[")
    ->right(manySepEnd(parseTerm(variableRegex), ~sep=lit(","), ~end_=lit("]")))
    ->map(vjs => Array(vjs))
  let parseKV = parseString->left(lit(":"))->and_(parseTerm(variableRegex))
  let parseObject =
    lit("{")
    ->right(manySepEnd(parseKV, ~sep=lit(","), ~end_=lit("}")))
    ->map(kvs => Object(kvs->Js.Dict.fromArray))

  let p =
    parseNull
    ->or_(parseBool)
    ->or_(parseNumber)
    ->or_(parseString->map(s => String(s)))
    ->or_(parseVariable(variableRegex))
    ->or_(parseObject)
    ->or_(parseArray)
  p(s)
}
