open VJsonTypes;

// Given a parser for a variable, produces a parser of VJson.
// For example, if you want your variable syntax to be `{{myVariable}}`, then
// you can do something like
//
// let parseVJson: ReludeParse.Parser.t(vjson(string)) =
//   parseVJsonWithVariable(
//     ReludeParse.Parser.(
//       regex([%re {|/[a-zA-Z_][a-zA-Z0-9_]*/|}])
//       |> between(str("{{"), str("}}"))
//     ),
//   );
//
let parseVJsonWithVariable:
  ReludeParse.Parser.t('v) => ReludeParse.Parser.t(vjson('v));

// Parses a variable in between double curly braces. The parser for the variable
// itself is up to the user.
//
// let parseVJson: ReludeParse.Parser.t(vjson(string)) =
//   parseVJsonWithDoubleCurlyBracesVariable(
//     ReludeParse.Parser.regex([%re {|/[a-zA-Z_][a-zA-Z0-9_]*/|}])
//   );
//
let parseVJsonWithDoubleCurlyBracesVariable:
  ReludeParse.Parser.t('v) => ReludeParse.Parser.t(vjson('v));
