// Some useful functions that operate on VJson
open VJsonTypes;

// Traverse a JSON structure with a function
let reduce: (vjson('v), 'a, ('a, vjson('v)) => 'a) => 'a;

// Translate to JSON, given a conversion function for a variable. Use
// sig-data-last for better compatibility with `@glennsl/bs-json`.
let toJson: ('v => Js.Json.t, vjson('v)) => Js.Json.t;

// Convert to a string of valid vjson syntax, using whatever method to serialize a variable.
// It's up to the user to guarantee that the variable name serializer produces valid output,
// so be careful.
let serialize: (vjson('v), 'v => string) => string;

// Traverse the tree, returning a set of all of the variables.
let findVariables: vjson('v) => array('v);
