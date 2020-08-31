// Some useful functions that operate on VJson
module Types = VJsonTypes;
include Types;
module Builder = VJsonBuilder;

// Traverse a JSON structure with a function
let rec map = (vjson, f) => {
  switch (vjson) {
  | Variable(v) => Variable(f(v))
  | Bool(b) => Bool(b)
  | Null => Null
  | String(s) => String(s)
  | Number(n) => Number(n)
  | Array(arr) => arr |> Builder.array(vj => vj->map(f))
  | Object(obj) => obj |> Builder.jsMap(k => k, vj => vj->map(f))
  };
};

// Traverse a JSON structure with a function
let rec reduce = (vjson, result, f) => {
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
    ->JsMap.valuesArray
    ->Belt.Array.reduce(newResult, (r, j) => j->reduce(r, f))
  };
};

// Translate to JSON, given a conversion function. Use sig-data-last for better
// compatibility with `@glennsl/bs-json`.
let rec toJson = variableToJson =>
  Json.Encode.(
    fun
    | Null => null
    | Bool(b) => b |> bool
    | Number(n) => n |> float
    | String(s) => s |> string
    | Array(arr) => arr |> array(toJson(variableToJson))
    | Object(d) => d |> JsMap.toJson(~k=s => s, ~v=toJson(variableToJson))
    | Variable(var) => var |> variableToJson
  );

// Convert to a string of valid vjson syntax, using whatever method to serialize a variable.
// It's up to the user to guarantee that the variable name serializer produces valid output,
// so be careful.
let rec serialize = (vj, variableToString) =>
  switch (vj) {
  | Null => "null"
  | Bool(true) => "true"
  | Bool(false) => "false"
  | Number(n) => n->Js.Float.toString
  | String(s) => Js.Json.(s->string->stringify)
  | Variable(v) => v->variableToString
  | Array(arr) =>
    "["
    ++ arr
       ->Belt.Array.map(vj' => vj'->serialize(variableToString))
       ->Js.Array2.joinWith(", ")
    ++ "]"
  | Object(o) =>
    "{"
    ++ o
       ->JsMap.toArray
       ->Belt.Array.map(((k, v)) =>
           Js.Json.(k->string->stringify)
           ++ ": "
           ++ v->serialize(variableToString)
         )
       ->Js.Array2.joinWith(", ")
    ++ "]"
  };

let serializeWrapCurlyBraces = (variableToString, vj) =>
  vj->serialize((v: 'v) => "{{" ++ v->variableToString ++ "}}");

let findVariables = root =>
  root->reduce(
    [||],
    (vars, vj) => {
      switch (vj) {
      | Variable(v) => vars->Js.Array2.push(v)->ignore
      | _ => ()
      };
      vars;
    },
  );

let parseInCurlyBraces:
  (ReludeParse.Parser.t('v), string) =>
  result(vjson('v), ReludeParse.Parser.ParseError.t) =
  (parseVariable, input) => {
    ReludeParse.Parser.(
      ws
      *> VJsonParse.parseVJsonWithVariable(
           parseVariable
           |> ReludeParse.Parser.(between(str("{{"), str("}}"))),
         )
      // Ensure that we have consumed the whole string
      <* eof
      |> runParser(input)
    );
  };

let defaultParseVariable =
  ReludeParse.Parser.regex([%re {|/[a-zA-Z_][a-zA-Z0-9_]*/|}]);
