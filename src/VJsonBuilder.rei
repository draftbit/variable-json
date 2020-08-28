open VJsonTypes;

let null: vjson('v);
let bool: bool => vjson('v);
let number: float => vjson('v);
let float: float => vjson('v);
let int: int => vjson('v);
let string: string => vjson('v);

// Create a VJson Variable from any variable type.
let variable: 'v => vjson('v);

// Returns its input. Useful if some of your data is already in VJson format.
let id: vjson('v) => vjson('v);

// VJson is a superset of JSON, so any JSON can be converted to VJson.
let json: Js.Json.t => vjson('v);

// You can use this to build an array with a conversion function
let array: ('a => vjson('v), array('a)) => vjson('v);

// If you already have an array of vjsonObjects, use this to combine them into one
let vjsonArray: array(vjson('v)) => vjson('v);

// Create a VJson object from a key/value array
let object_: array((string, vjson('v))) => vjson('v);

// Use this to convert a dictionary of objects into a VJson object.
let dict: ('a => vjson('v), Js.Dict.t('a)) => vjson('v);

// Use this to convert a Map of objects into a VJson object.
let jsMap: ('k => string, 'a => vjson('v), JsMap.t('k, 'a)) => vjson('v);
