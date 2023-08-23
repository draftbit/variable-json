type rec vjson<'v> =
  | Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Variable('v)
  | Array(array<vjson<'v>>)
  | Object(JsMap.t<string, vjson<'v>>)

type t<'v> = vjson<'v>
