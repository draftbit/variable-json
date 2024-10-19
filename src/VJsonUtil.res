let mapDict = (dict, f) =>
  dict->Js.Dict.entries->Js.Array2.map(((k, v)) => (k, f(v)))->Js.Dict.fromArray
let keepMapDict = (dict, f) =>
  dict
  ->Js.Dict.entries
  ->Belt.Array.keepMap(((k, v)) => f(v)->Belt.Option.map(v2 => (k, v2)))
  ->Js.Dict.fromArray
let quote: string => string = s => s->Js.Json.string->Js.Json.stringify

@gentype
let tapLog = (value, description) => {
  Js.log2(description, value)
  value
}

let renderLineAndColumn = ((line, column)) =>
  `line ${line->Js.Int.toString}, column ${column->Js.Int.toString}`

// Given an index in a string, return the line and column number at that index.
let getLineAndColumn: (string, int) => option<(int, int)> = (str, index) =>
  if index >= str->Js.String.length || index < 0 {
    None
  } else {
    let substr = str->Js.String2.slice(~from=0, ~to_=index)
    let lines = substr->Js.String2.split("\n")
    let nLines = lines->Js.Array2.length
    Some((
      nLines,
      lines->Belt.Array.get(nLines - 1)->Belt.Option.mapWithDefault(0, Js.String.length),
    ))
  }
