// Serialize a Map to JSON. This uses significant-data-last ordering for
// mixing with encoders in `@glennsl/bs-json` (although it works standalone)
let mapToJson = (innerToJson, m): Js.Json.t =>
  m->JsMap.map(innerToJson)->JsMap.toDict->Js.Json.object_;

let variableRegex = [%re {|/[a-zA-Z_][a-zA-Z0-9_]*/|}];
module VariableName =
  Opaque.String.Make(
    (
      Opaque.String.Validation.MatchRegex({
        let regex = variableRegex;
      })
    ),
    {},
  );

type vjson =
  | Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Variable(VariableName.t)
  | Array(array(vjson))
  | Object(JsMap.t(string, vjson));

// Shorthand
type t = vjson;

// Traverse a JSON structure with a function
let rec reduce: 'a. (vjson, 'a, ('a, vjson) => 'a) => 'a =
  (vjson, result, f) => {
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

// Parse from JSON (the result will have no Variable variants)
let rec fromJson = json => {
  switch (json->Js.Json.classify) {
  | JSONFalse => Bool(false)
  | JSONTrue => Bool(true)
  | JSONNull => Null
  | JSONString(s) => String(s)
  | JSONNumber(n) => Number(n)
  | JSONArray(arr) => Array(arr->Belt.Array.map(fromJson))
  | JSONObject(obj) => Object(obj->JsMap.fromDict->JsMap.map(fromJson))
  };
};

// Translate to JSON, given a conversion function.
let rec toJson = (variableToJson: VariableName.t => Js.Json.t) =>
  Json.Encode.(
    fun
    | Null => null
    | Bool(b) => b |> bool
    | Number(n) => n |> float
    | String(s) => s |> string
    | Array(arr) => arr |> array(toJson(variableToJson))
    | Object(d) => d |> mapToJson(toJson(variableToJson))
    | Variable(var) => var |> variableToJson
  );

// Convert to a string of valid vjson syntax.
let toString: t => string = _ => failwith("");

// Traverse the tree, returning a set of all of the variable names.
let findVariables: t => JsSet.t(VariableName.t) =
  root =>
    root->reduce(JsSet.empty(), (vars, v) =>
      switch (v) {
      | Variable(vn) => vars->JsSet.addMut(vn)
      | _ => vars
      }
    );
