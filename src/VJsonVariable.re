type config('variable) = {
  // Serialize a variable to a string.
  variableToString: 'variable => string,
  parse: ReludeParse.Parser.t('variable),
};

// Provide a regex for a variable. It's up to the user to make sure
// that this regex does *not* match the literal '}}'.
let regexConfig: Js.Re.t => config(string) =
  regex => {
    variableToString: s => s,
    parse: ReludeParse.Parser.regex(regex),
  };

let parseWithConfig:
  (config('v), string) =>
  result(VJsonTypes.vjson('v), ReludeParse.Parser.ParseError.t) =
  ({parse}, input) => {
    ReludeParse.Parser.(
      ws
      *> VJsonParse.parseVJsonWithDoubleCurlyBracesVariable(parse)
      <* eof
      |> runParser(input)
    );
  };

let toString: (config('v), VJson.t('v)) => string =
  ({variableToString}, vj) =>
    vj->VJsonOperations.serialize((v: 'v) =>
      "{{" ++ v->variableToString ++ "}}"
    );
