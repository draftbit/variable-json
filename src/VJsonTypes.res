type rec vjson =
  | Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Variable(string)
  | Array(array<vjson>)
  | Object(Js.Dict.t<vjson>)

type t = vjson
