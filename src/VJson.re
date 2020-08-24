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

// Convenient builder functions. Sig-data-last style a la bs-json.
module Build = {
  let null = Null;
  let bool = b => Bool(b);
  let number = n => Number(n);
  let float = f => Number(f);
  let int = i => Number(i->float_of_int);
  let string = s => String(s);
  let variable = v => Variable(v);
  let id = vj => vj;

  // You can use this to build an array with a conversion function
  let array = (toVJson, arr) => Array(arr->Belt.Array.map(toVJson));

  // If you already have an array of vjsonObjects, use this to combine them into one
  let vjsonArray = arr => Array(arr);

  // Create a VJson object from a key/value array
  let object_ = keyVals => Object(keyVals->JsMap.fromArray);

  // Use this to convert a dictionary of objects into a VJson object.
  let dict = (toVJson, dict) =>
    Object(dict->JsMap.fromDict->JsMap.map(toVJson));
};

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

// Translate to JSON, given a conversion function. Use sig-data-last for better
// compatibility with `@glennsl/bs-json`.
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

// Convert to a string of valid vjson syntax, using whatever method to serialize a variable.
// It's up to the user to guarantee that the variable name serializer produces valid output,
// so be careful.
let rec toString: (t, VariableName.t => string) => string =
  (vj, variableToString) =>
    switch (vj) {
    | Null => "null"
    | Bool(true) => "true"
    | Bool(false) => "false"
    // TODO I don't want to use Obj.magic here but for some reason Js.Json.float expects an int
    | Number(n) => Obj.magic(n)->Js.Json.stringify
    | String(s) => Js.Json.(s->string->stringify)
    | Variable(v) => "{{" ++ v->variableToString ++ "}}"
    | Array(arr) =>
      "["
      ++ arr
         ->Belt.Array.map(vj' => vj'->toString(variableToString))
         ->Js.Array2.joinWith(", ")
      ++ "]"
    | Object(o) =>
      "{"
      ++ o
         ->JsMap.toArray
         ->Belt.Array.map(((k, v)) =>
             Js.Json.(k->string->stringify)
             ++ ": "
             ++ v->toString(variableToString)
           )
         ->Js.Array2.joinWith(", ")
      ++ "]"
    };

// Traverse the tree, returning a set of all of the variable names.
let findVariables: t => JsSet.t(VariableName.t) =
  root =>
    root->reduce(JsSet.empty(), (vars, v) =>
      switch (v) {
      | Variable(vn) => vars->JsSet.addMut(vn)
      | _ => vars
      }
    );
