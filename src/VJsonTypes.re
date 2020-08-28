type vjson('v) =
  | Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Variable('v)
  | Array(array(vjson('v)))
  | Object(JsMap.t(string, vjson('v)));

// Traverse a JSON structure with a function
let rec reduce: 'a 'v. (vjson('v), 'a, ('a, vjson('v)) => 'a) => 'a =
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

// Translate to JSON, given a conversion function. Use sig-data-last for better
// compatibility with `@glennsl/bs-json`.
let rec toJson = (variableToJson: 'v => Js.Json.t) =>
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
let rec toString: (vjson('v), 'v => string) => string =
  (vj, variableToString) =>
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

// Traverse the tree, returning a set of all of the variables.
let findVariables: vjson('v) => array('v) =
  root =>
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
