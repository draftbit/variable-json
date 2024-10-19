open VJsonTypes
open StringParser

let lit = s => literal(s)->lexeme

let parseNull: parser<vjson> = lit("null")->map(_ => Null)

let parseBool: parser<vjson> = lit("true")->or_(lit("false"))->map(s => Bool(s === "true"))

let parseNumber: parser<vjson> =
  regex(Regexes.number, "a number")->map(s => Number(s->Js.Float.fromString))->lexeme

let parseString: parser<string> =
  regex(Regexes.string, ~index=1, "a string literal")
  ->map(str => str->Js.String2.replaceByRe(Regexes.escapedQuote, "\""))
  ->lexeme

let parseVariable: parser<vjson> =
  between(lit("{{"), regex(Regexes.variable, "a variable")->lexeme, lit("}}"))->map(v => Variable(
    v,
  ))

let rec parseTerm: parser<vjson> = s => {
  let p =
    parseNull
    ->or_(parseBool)
    ->or_(parseNumber)
    ->or_(parseString->map(s => String(s)))
    ->or_(parseVariable)
    ->or_(parseArray)
    ->or_(parseObject)
  p(s)
}
and parseArray = s => {
  let p = between(lit("["), manySep(parseTerm, lit(",")), lit("]"))->map(vjs => Array(vjs))
  p(s)
}
and parseKV = s => (parseString->left(lit(":"))->and_(parseTerm))(s)
and parseObject = s =>
  (
    between(lit("{"), manySep(parseKV, lit(",")), lit("}"))->map(kvs => Object(
      kvs->Js.Dict.fromArray,
    ))
  )(s)
