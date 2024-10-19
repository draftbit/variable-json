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
let between: 'a 'b 'c. (parser<'a>, parser<'b>, parser<'c>) => parser<'b> = (l, parser, r) =>
  l->and_(parser)->and_(r)->map((((_, res), _)) => res)

let many: 'a. parser<'a> => parser<array<'a>> = parser => startStr => {
  let results = []
  let str = ref(startStr)
  let keepGoing = ref(true)
  while keepGoing.contents {
    switch str.contents->parser {
    | Ok({str: next, res}) => {
        results->Js.Array2.push(res)->ignore
        str := next
      }
    | Error(_) => keepGoing := false
    }
  }
  Ok({str: str.contents, res: results})
}

let many1: 'a. parser<'a> => parser<array<'a>> = parser =>
  parser->and_(many(parser))->map(((first, rest)) => [first]->Js.Array2.concat(rest))

let manySep: 'a. (parser<'a>, parser<_>) => parser<array<'a>> = (parser, sepParser) => startStr => {
  let results = []
  let str = ref(startStr)
  let keepGoing = ref(true)
  while keepGoing.contents {
    switch str.contents->parser {
    | Ok({str: next, res}) => {
        results->Js.Array2.push(res)->ignore
        switch next->sepParser {
        | Ok({str: next2}) => str := next2
        | _ => {
            str := next
            keepGoing := false
          }
        }
      }
    | Error(_) => keepGoing := false
    }
  }
  Ok({str: str.contents, res: results})
}

let sliceN = (str, from) => str->Js.String2.sliceToEnd(~from)

let literal: string => parser<string> = expected => str =>
  str->Js.String2.startsWith(expected)
    ? Ok({str: str->sliceN(expected->Js.String.length), res: expected})
    : Error({expected, remaining: str})

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

let whitespace: parser<string> = regex(%re(`/^\s*/`), "whitespace")

let lexeme: parser<'a> => parser<'a> = parser => left(parser, whitespace)

let eof: parser<unit> = str =>
  str === "" ? Ok({str: "", res: ()}) : Error({expected: "end of file", remaining: str})
