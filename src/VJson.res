// Some useful functions that operate on VJson
module Types = VJsonTypes
include Types
module Builder = VJsonBuilder

// Traverse a JSON structure with a function
let rec map = (vjson, f) =>
  switch vjson {
  | Variable(v) => Variable(f(v))
  | Bool(b) => Bool(b)
  | Null => Null
  | String(s) => String(s)
  | Number(n) => Number(n)
  | Array(arr) => arr |> Builder.array(vj => vj->map(f))
  | Object(obj) => obj |> Builder.jsMap(k => k, vj => vj->map(f))
  }

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
  | Object(obj) => obj->JsMap.valuesArray->Belt.Array.reduce(newResult, (r, j) => j->reduce(r, f))
  }
}

let reduceL = (start, f, vjson) => reduce(vjson, start, f)

// Translate to JSON, given a conversion function which must return a JSON value for each variable.
let rec toJson = variableToJson => {
  open Json.Encode

  x =>
    switch x {
    | Null => null
    | Bool(b) => b |> bool
    | Number(n) => n |> float
    | String(s) => s |> string
    | Array(arr) => arr |> array(toJson(variableToJson))
    | Object(d) => d |> JsMap.toJson(~k=s => s, ~v=toJson(variableToJson))
    | Variable(var) => var |> variableToJson
    }
}

// Translate to JSON, given a conversion function which will optionally return a JSON value for each variable.
let rec toJsonOptional: (
  'v => option<Js.Json.t>,
  vjson<'v>,
) => option<Js.Json.t> = variableToJson => {
  open Json.Encode

  x =>
    switch x {
    | Null => null->Some
    | Bool(b) => b->bool->Some
    | Number(n) => n->float->Some
    | String(s) => s->string->Some
    | Array(arr) =>
      arr
      ->Js.Array2.map(vj =>
        vj->toJsonOptional(variableToJson, _)->Belt.Option.getWithDefault(Js.Json.null)
      )
      ->Js.Json.array
      ->Some
    | Object(d) =>
      d->JsMap.keepMap(toJsonOptional(variableToJson))->JsMap.toDict->Js.Json.object_->Some
    | Variable(var) => var->variableToJson
    }
}

let quote: string => string = s => s->Js.Json.string->Js.Json.stringify

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
    ->JsMap.toArray
    ->Belt.Array.map(((k, v)) => `${k->quote}: ${v->serialize(variableToString)}`)
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

let defaultVariableRegex = %re(`/[a-zA-Z_][a-zA-Z0-9_]*/`)

let parseWith = (parseVariable, input) => {
  open ReludeParse.Parser
  // Ensure that we have consumed the whole string
  \"<*"(\"*>"(ws, VJsonParse.parseVJsonWithVariable(parseVariable)), eof) |> runParser(input)
}

let parseWithExn = (parseVariable, input) =>
  switch parseWith(parseVariable, input) {
  | Ok(vjson) => vjson
  | Error(ReludeParse.Parser.ParseError.ParseError(message)) => failwith(message)
  }

// Turn a regex into a string parser using relude-parse
let parseFromRegex: (Js.Re.t, string) => result<string, string> = (reg, myString) =>
  switch {
    open ReludeParse.Parser
    \"<*"(regex(reg), eof) |> runParser(myString)
  } {
  | Ok(variable) => Ok(variable)
  | Error(ParseError(m)) => Error(m)
  }

let defaultParseVariable = parseFromRegex(defaultVariableRegex)
let parseAnyStringVariable = s => Ok(s)
let parseDefault = parseWith(defaultParseVariable)
let parseDefaultExn = parseWithExn(defaultParseVariable)
let parseWithRegex = regex => parseWith(parseFromRegex(regex))
