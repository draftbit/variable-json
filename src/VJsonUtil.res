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
