// Some useful functions that operate on VJson
type vjson('v) = VJsonTypes.vjson('v);

// Abbreviation
type t('v) = vjson('v);

// Alter every variable in a VJson tree
let map: (vjson('v1), 'v1 => 'v2) => vjson('v2);

// Traverse a JSON structure with a function
let reduce: (vjson('v), 'a, ('a, vjson('v)) => 'a) => 'a;

// Translate to JSON, given a conversion function for a variable. Use
// sig-data-last for better compatibility with `@glennsl/bs-json`.
let toJson: ('v => Js.Json.t, vjson('v)) => Js.Json.t;

// Convert to a string of valid vjson syntax, using whatever method to serialize a variable.
// It's up to the user to guarantee that the variable name serializer produces valid output,
// so be careful.
let serialize: (vjson('v), 'v => string) => string;

// Serializes into a pure, JSON-like syntax, wrapping variables in
// pairs of curly braces. The serialization of the variable itself
// is user defined. In many cases the identify function `s => s` works fine.
// If you want to create a string without wrapping variables at all, you
// can use `serialize.` As the inverse of parsing, take care that
// this function never includes `}}` (TODO: support escape sequences)
let serializeWrapCurlyBraces: ('v => string, vjson('v)) => string;

// Traverse the tree, returning a set of all of the variables.
let findVariables: vjson('v) => array('v);

// Parse with a custom variable parser.
let parseInCurlyBraces:
  (ReludeParse.Parser.t('v), string) =>
  result(vjson('v), ReludeParse.Parser.ParseError.t);

// This is a parser for a basic alphanumeric identifier variable, such as
// `My_variable123`. It covers a lot of use cases, but you can build
// your own as well. Note that this parser only parses what's *between*
// pairs of curly braces. This means though that it is up to the user
// to ensure that the parser doesn't consume `}}` as input. This limitation
// will likely change in the future.
let defaultParseVariable: ReludeParse.Parser.t(string);
