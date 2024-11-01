// Some useful functions that operate on VJson
include VJsonTypes
module Builder = VJsonBuilder
open VJsonUtil

// Map over a JSON structure with a function operating on variables
let rec map = (vjson, f) =>
  switch vjson {
  | Variable(v) => Variable(f(v))
  | Bool(b) => Bool(b)
  | Null => Null
  | String(s) => String(s)
  | Number(n) => Number(n)
  | Array(arr) => Builder.array(vj => vj->map(f), arr)
  | Object(obj) => Builder.dict(vj => vj->map(f), obj)
  }

// `map` in significant-data-last order
let mapL = (f, vjson) => map(vjson, f)

// Traverse a JSON structure with a function
let rec reduce = (vjson, result, f) => {
  let newResult = f(result, vjson)
  switch vjson {
  | Bool(_)
  | Null
  | String(_)
  | Number(_)
  | Variable(_) => newResult
  | Array(arr) => arr->Belt.Array.reduce(newResult, (r, j) => j->reduce(r, f))
  | Object(obj) => obj->Js.Dict.values->Belt.Array.reduce(newResult, (r, j) => j->reduce(r, f))
  }
}

// `reduce` in significant-data-last order
let reduceL = (start, f, vjson) => reduce(vjson, start, f)

// Translate to JSON, given a conversion function which must return a JSON value for each variable.
let rec toJson = variableToJson => {
  x =>
    switch x {
    | Null => Js.Json.null
    | Bool(b) => Js.Json.boolean(b)
    | Number(n) => Js.Json.number(n)
    | String(s) => Js.Json.string(s)
    | Array(arr) => arr->Js.Array2.map(toJson(variableToJson))->Js.Json.array
    | Object(d) => d->mapDict(toJson(variableToJson))->Js.Json.object_
    | Variable(var) => variableToJson(var)
    }
}

// Translate to JSON, given a conversion function which will optionally return a JSON value for each variable.
let rec toJsonOptional: 'v. ('v => option<Js.Json.t>, vjson<'v>) => option<Js.Json.t> = (
  variableToJson,
  x,
) => {
  switch x {
  | Null => Js.Json.null->Some
  | Bool(b) => b->Js.Json.boolean->Some
  | Number(n) => n->Js.Json.number->Some
  | String(s) => s->Js.Json.string->Some
  | Array(arr) =>
    arr
    ->Js.Array2.map(vj =>
      vj->toJsonOptional(variableToJson, _)->Belt.Option.getWithDefault(Js.Json.null)
    )
    ->Js.Json.array
    ->Some
  | Object(d) => d->keepMapDict(vj => toJsonOptional(variableToJson, vj))->Js.Json.object_->Some
  | Variable(var) => var->variableToJson
  }
}

// Convert to a string of valid vjson syntax, using whatever method to serialize a variable.
// It's up to the user to guarantee that the variable name serializer produces valid output,
// so be careful.
let rec serialize = (vj, variableToString) =>
  switch vj {
  | Null => "null"
  | Bool(true) => "true"
  | Bool(false) => "false"
  | Number(n) => n->Js.Float.toString
  | String(s) => s->quote
  | Variable(v) => v->variableToString
  | Array(arr) =>
    `[${arr->Belt.Array.map(vj' => vj'->serialize(variableToString))->Js.Array2.joinWith(", ")}]`
  | Object(o) =>
    "{" ++
    (o
    ->Js.Dict.entries
    ->Js.Array2.map(((k, v)) => `${k->quote}: ${v->serialize(variableToString)}`)
    ->Js.Array2.joinWith(", ") ++
    "}")
  }

let serializeWrapCurlyBraces = (variableToString, vj) =>
  vj->serialize((v: 'v) => "{{" ++ (v->variableToString ++ "}}"))

let findVariables = root =>
  root->reduce([], (vars, vj) => {
    switch vj {
    | Variable(v) => vars->Js.Array2.push(v)->ignore
    | _ => ()
    }
    vars
  })

let defaultVariableRegex = Regexes.variable

let parse = (~variableRegex=defaultVariableRegex, s) => {
  let p =
    StringParser.whitespace
    ->StringParser.right(VJsonParse.parseTerm(variableRegex))
    ->StringParser.left(StringParser.eof)
  switch s->p {
  | Ok(r) => Ok(r.res)
  | Error(e) => {
      let posn = s->Js.String.length - e.remaining->Js.String.length - 1
      let location = switch getLineAndColumn(s, posn) {
      | Some(lineAndCol) => renderLineAndColumn(lineAndCol)
      | None => `position ${posn->Js.Int.toString}`
      }
      let remaining =
        e.remaining->Js.String2.replaceByRe(%re(`/\s+/`), " ")->Js.String2.slice(~from=0, ~to_=20)
      let remaining = remaining === "" ? "end of file" : remaining->quote
      Error(`At ${location}: Expected ${e.expected} but got ${remaining}`)
    }
  }
}
