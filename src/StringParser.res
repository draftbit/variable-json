open VJsonUtil

type success<'res> = {str: string, res: 'res}
type failure = {expected: string, remaining: string}
type parser<'res> = string => result<success<'res>, failure>

let map: (parser<'a>, 'a => 'b) => parser<'b> = (parser, f) => str =>
  str->parser->Belt.Result.map(({str, res}) => {str, res: f(res)})

let opt: parser<'a> => parser<option<'a>> = parser => str =>
  switch str->parser {
  | Ok({str: str2, res}) => Ok({str: str2, res: Some(res)})
  | _ => Ok({str, res: None})
  }

let or_: (parser<'a>, parser<'a>) => parser<'a> = (parser1, parser2) => str =>
  switch str->parser1 {
  | Ok(res) => Ok(res)
  | Error(e1) =>
    switch str->parser2 {
    | Ok(res) => Ok(res)
    // Prefer the error message of whichever one consumed more input
    | Error(e2) => Error(e1.remaining->Js.String.length < e2.remaining->Js.String.length ? e1 : e2)
    }
  }

let and_: (parser<'a>, parser<'b>) => parser<('a, 'b)> = (parser1, parser2) => str1 =>
  switch str1->parser1 {
  | Ok({str: str2, res: res1}) =>
    str2->parser2->Belt.Result.map(({str: str3, res: res2}) => {str: str3, res: (res1, res2)})
  | Error(e) => Error(e)
  }

let left: (parser<'a>, parser<_>) => parser<'a> = (parser1, parser2) =>
  parser1->and_(parser2)->map(fst)
let right: (parser<_>, parser<'a>) => parser<'a> = (parser1, parser2) =>
  parser1->and_(parser2)->map(snd)

// Parse as many items of the first parser as possible. After each parse, attempt to
// parse a separator. If successful, keep parsing. Otherwise, parse with the end parser.
let manySepEnd: 'a. (parser<'a>, ~sep: parser<_>, ~end_: parser<_>) => parser<array<'a>> = (
  parser,
  ~sep,
  ~end_,
) => startStr => {
  let results = []
  let str = ref(startStr)
  let keepGoing = ref(true)
  let error = ref(None)
  let endLoop = (lastError, ~combine) => {
    keepGoing := false
    switch str.contents->end_ {
    // The results we parsed are only valid if the endParser also matches.
    | Ok({str: next}) => str := next
    // Otherwise, the last failure is the "real" error.
    | Error(e) =>
      error :=
        Some(
          combine
            ? {...lastError, expected: lastError.expected->concatIfNotIncluded(` OR ${e.expected}`)}
            : lastError,
        )
    }
  }

  while keepGoing.contents {
    switch str.contents->parser {
    | Ok({str: next, res}) => {
        results->Js.Array2.push(res)->ignore
        switch next->sep {
        | Ok({str: next2}) => str := next2
        | Error(e) => {
            str := next
            // After a successful item parse, either a separator or an end parse is
            // valid, so if both fail, combine their errors
            endLoop(e, ~combine=true)
          }
        }
      }
    // If the item parse fails, only an end parse is valid, don't combine errors
    | Error(e) => endLoop(e, ~combine=false)
    }
  }
  switch error.contents {
  | None => Ok({str: str.contents, res: results})
  | Some(e) => Error(e)
  }
}

let sliceN = (str, from) => str->Js.String2.sliceToEnd(~from)

let literal: string => parser<string> = expected => str =>
  str->Js.String2.startsWith(expected)
    ? Ok({str: str->sliceN(expected->Js.String.length), res: expected})
    : Error({expected: expected->quote, remaining: str})

let regex: (Js.Re.t, ~index: int=?, string) => parser<string> = (
  regex,
  // Which index of the returned regex capture array to return
  ~index=0,
  // Used to generate error message if the parser fails
  description,
) => str =>
  switch str->Js.String2.match_(regex) {
  | Some(arr) =>
    Ok({
      str: str->sliceN(arr[0]->Belt.Option.getExn->Js.String.length),
      res: arr[index]->Belt.Option.getExn,
    })
  | _ => Error({expected: description, remaining: str})
  }

let whitespace: parser<string> = regex(Regexes.whitespace, "whitespace")

let lexeme: parser<'a> => parser<'a> = parser => left(parser, whitespace)

let eof: parser<unit> = str =>
  str === "" ? Ok({str: "", res: ()}) : Error({expected: "end of file", remaining: str})
