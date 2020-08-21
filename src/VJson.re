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
  | Object(Js.Dict.t(vjson));

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
      ->Js.Dict.values
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
  | JSONObject(obj) =>
    Object(
      obj
      ->Js.Dict.entries
      ->Belt.Array.map(((k, v)) => (k, fromJson(v)))
      ->Js.Dict.fromArray,
    )
  };
};

// Translate to JSON, given a conversion function.
let rec toJson = variableToJson =>
  Json.Encode.(
    fun
    | Null => null
    | Bool(b) => b |> bool
    | Number(n) => n |> float
    | String(s) => s |> string
    | Array(arr) => arr |> array(toJson(variableToJson))
    | Object(d) => d |> dict(toJson(variableToJson))
    | Variable(var) => var |> variableToJson
  );

// Traverse the tree, returning a set of all of the variable names.
let findVariables: vjson => JsSet.t(VariableName.t) =
  root =>
    root->reduce(JsSet.empty(), (vars, v) =>
      switch (v) {
      | Variable(vn) => vars->JsSet.addMut(vn)
      | _ => vars
      }
    );
