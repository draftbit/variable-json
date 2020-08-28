include VJsonTypes;

let null = Null;
let bool = b => Bool(b);
let number = n => Number(n);
let float = f => Number(f);
let int = i => Number(i->float_of_int);
let string = s => String(s);
let variable = v => Variable(v);
let id = vj => vj;

let array = (toVJson, arr) => Array(arr->Belt.Array.map(toVJson));

let vjsonArray = arr => Array(arr);

let object_ = keyVals => Object(keyVals->JsMap.fromArray);

let dict = (toVJson, dict) =>
  Object(dict->JsMap.fromDict->JsMap.map(toVJson));

let jsMap = (keyToString, vToVJson, m) =>
  Object(m->JsMap.mapEntries((k, v) => (k->keyToString, v->vToVJson)));

let rec json = j => {
  switch (j->Js.Json.classify) {
  | JSONFalse => Bool(false)
  | JSONTrue => Bool(true)
  | JSONNull => Null
  | JSONString(s) => String(s)
  | JSONNumber(n) => Number(n)
  | JSONArray(arr) => Array(arr->Belt.Array.map(json))
  | JSONObject(obj) => Object(obj->JsMap.fromDict->JsMap.map(json))
  };
};
