type t('v) = VJsonTypes.vjson('v);

let null: t('v);
let bool: bool => t('v);
let number: float => t('v);
let float: float => t('v);
let int: int => t('v);
let string: string => t('v);
let variable: 'v => t('v);
let id: t('v) => t('v);
let json: Js.Json.t => t('v);

// You can use this to build an array with a conversion function
let array: ('a => t('v), array('a)) => t('v);

// If you already have an array of vjsonObjects, use this to combine them into one
let vjsonArray: array(t('v)) => t('v);

// Create a VJson object from a key/value array
let object_: array((string, t('v))) => t('v);

// Use this to convert a dictionary of objects into a VJson object.
let dict: ('a => t('v), Js.Dict.t('a)) => t('v);
