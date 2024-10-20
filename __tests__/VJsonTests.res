open Jest
open Expect
open VJson

let exampleJson: Js.Json.t = %raw(`{x: 1, y: 2, z: [1, 2, "hello"], q: { x: [false, true], y: null }}`)

let example = {
  open VJson
  parse("{
       \"id\": {{id}},
       \"color\": {{color}},
       \"size\": {{size}},
     }")
}->Belt.Result.getExn

let moreComplexExample = {
  open VJson
  parse("{
       \"id\": {{id}},
       \"color\": {{color}},
       \"size\": {{size}},
       \"array\": [{{x}}, {{y}}],
       \"obj\": {
         \"q\": 3,
         \"z\": {{z}}
       }
     }")
}->Belt.Result.getExn

module FindVariablesTests = {
  describe("findVariables", () =>
    test("it finds variables", () =>
      expect(example->findVariables)->toEqual(["id", "color", "size"])
    )
  )
}

module MapTests = {
  describe("map a function over VJson", () =>
    test("complex with variables", () => {
      open VJson.Builder
      let vj = object_([
        ("x", variable("1234")),
        ("z", vjsonArray([float(1.0), number(2.0), variable("4321")])),
      ])
      expect(vj->VJson.map(int_of_string))->toEqual(
        object_([
          ("x", variable(1234)),
          ("z", vjsonArray([float(1.0), number(2.0), variable(4321)])),
        ]),
      )
    })
  )
}

module SerializeTests = {
  let expectSerialize = (vj: vjson<string>, str) => {
    let serialized: string = VJson.serializeWrapCurlyBraces(s => s, vj)
    expect(serialized)->toEqual(str)
  }
  test("simple values", () => {
    expectSerialize(Bool(true), "true")
    expectSerialize(String("hello"), "\"hello\"")
    expectSerialize(Variable("varvara"), "{{varvara}}")
  })

  test("complex json", () =>
    expect(exampleJson->VJson.Builder.json->VJson.serialize(s => s))->toMatchSnapshot()
  )

  test("complex with variables", () => {
    let vj = {
      open VJson.Builder
      object_([
        ("x", float(1.0)),
        ("y", int(2)),
        ("z", vjsonArray([float(1.0), number(2.0), variable("vivvy")])),
        ("q", object_([("x", Array([Bool(false), "oogabooga"->variable])), ("y", variable("yo"))])),
      ])
    }
    expect(vj->VJson.serialize(s => s))->toMatchSnapshot()
    let serialized: string = VJson.serializeWrapCurlyBraces(s => s, vj)
    expect(serialized->VJson.parse->Belt.Result.getExn)->toEqual(vj)
  })

  test("serializing VJson without variables produces valid json", () => {
    let vj = {
      open VJson.Builder
      object_([
        ("x", float(1.0)),
        ("y", int(2)),
        ("z", vjsonArray([float(1.0), number(2.0)])),
        ("q", object_([("x", Array([Bool(false)]))])),
      ])
    }
    let serialized: string = VJson.serializeWrapCurlyBraces(s => s, vj)
    expect(Obj.magic(Js.Json.parseExn(serialized)))->toEqual({
      "x": 1.0,
      "y": 2,
      "z": [1.0, 2.0],
      "q": {
        "x": [false],
      },
    })
  })
}

module FromJsonTests = {
  describe("fromJson", () =>
    test("nested object", () =>
      expect(exampleJson->VJson.Builder.json)->toEqual(
        Object(
          {
            open VJson.Builder
            [
              ("x", number(1.0)),
              ("y", number(2.0)),
              ("z", array(id, [number(1.0), number(2.0), string("hello")])),
              ("q", object_([("x", array(bool, [false, true])), ("y", null)])),
            ]
          }->Js.Dict.fromArray,
        ),
      )
    )
  )
}

module ToJsonTests = {
  describe("toJson", () => {
    let variableToJson: string => Js.Json.t = v =>
      switch v {
      | "id" => Js.Json.number(123.0)
      | "color" => Js.Json.string("pink")
      | _ => Js.Json.null
      }

    expect(toJson(variableToJson)(example))->toEqual(%raw(`{id: 123, size: null, color: "pink"}`))

    let variableToJson: string => Js.Json.t = v =>
      switch v {
      | "id" => Js.Json.number(123.0)
      | "color" => Js.Json.string("pink")
      | "x" => Js.Json.number(-5.0)
      | "y" => Js.Json.boolean(false)
      | "z" => Js.Json.string("yo")
      | _ => Js.Json.null
      }

    expect(toJson(variableToJson)(moreComplexExample))->toEqual(
      %raw(`{id: 123, size: null, color: "pink", array: [-5, false], obj: {q: 3, z: "yo"}}`),
    )
  })

  describe("toJsonOptional", () => {
    let variableToJson: string => option<Js.Json.t> = v =>
      switch v {
      | "id" => Js.Json.number(123.0)->Some
      | "color" => Js.Json.string("pink")->Some
      | _ => None
      }

    expect(toJsonOptional(variableToJson, example))->toEqual(%raw(`{id: 123, color: "pink"}`))

    let variableToJson = v =>
      switch v {
      | "id" => Js.Json.number(123.0)->Some
      | "color" => Js.Json.string("blue")->Some
      | "y" => Js.Json.boolean(false)->Some
      | _ => None
      }

    expect(toJsonOptional(variableToJson, moreComplexExample))->toEqual(
      %raw(`{id: 123, array: [null, false], color: "blue", obj: { q: 3}}`),
    )
  })
}
