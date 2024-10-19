type parser<'res> = string => result<(string, 'res), string>

let map: (parser<'a>, 'a => 'b) => parser<'b> = (parser, f) => str =>
  str->parser->Belt.Result.map(((str, res)) => (str, f(res)))

let opt: parser<'a> => parser<option<'a>> = parser => str =>
  switch str->parser {
  | Ok((str2, res)) => Ok((str2, Some(res)))
  | _ => Ok(str, None)
  }

let or_: (parser<'a>, parser<'a>) => parser<'a> = (parser1, parser2) => str =>
  switch str->parser1 {
  | Ok(res) => Ok(res)
  | Error(_) => str->parser2
  }

let and_: (parser<'a>, parser<'b>) => parser<('a, 'b)> = (parser1, parser2) => str1 =>
  switch str1->parser1 {
  | Ok((str2, res1)) => str2->parser2->Belt.Result.map(((str3, res2)) => (str3, (res1, res2)))
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
    | Ok((next, res)) => {
        results->Js.Array2.push(res)->ignore
        str := next
      }
    | Error(_) => keepGoing := false
    }
  }
  Ok((str.contents, results))
}

let many1: 'a. parser<'a> => parser<array<'a>> = parser =>
  parser->and_(many(parser))->map(((first, rest)) => [first]->Js.Array2.concat(rest))

let manySep: 'a. (parser<'a>, parser<_>) => parser<array<'a>> = (parser, sepParser) => startStr => {
  let results = []
  let str = ref(startStr)
  let keepGoing = ref(true)
  while keepGoing.contents {
    switch str.contents->parser {
    | Ok((next, res)) => {
        results->Js.Array2.push(res)->ignore
        switch next->sepParser {
        | Ok((next2, _)) => str := next2
        | _ => {
            str := next
            keepGoing := false
          }
        }
      }
    | Error(_) => keepGoing := false
    }
  }
  Ok((str.contents, results))
}

let sliceN = (str, from) => str->Js.String2.sliceToEnd(~from)

let literal: string => parser<string> = expected => str =>
  str->Js.String2.startsWith(expected)
    ? Ok((str->sliceN(expected->Js.String.length), expected))
    : Error(`expected "${expected}"`)

let regex: (Js.Re.t, ~index: int=?, string) => parser<string> = (
  regex,
  // Which index of the returned regex capture array to return
  ~index=0,
  // Used to generate error message if the parser fails
  description,
) => str =>
  switch str->Js.String2.match_(regex) {
  | Some(arr) =>
    Ok(str->sliceN(arr[0]->Belt.Option.getExn->Js.String.length), arr[index]->Belt.Option.getExn)
  | _ => Error(`expected ${description}`)
  }

let whitespace: parser<string> = regex(%re(`/^\s*/`), "whitespace")

let lexeme: parser<'a> => parser<'a> = parser => left(parser, whitespace)

let eof: parser<unit> = str => str === "" ? Ok("", ()) : Error("expected end of file")
