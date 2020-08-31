open Jest;
open Expect;
open VJson;

let exampleJson: Js.Json.t = [%raw
  {|{x: 1, y: 2, z: [1, 2, "hello"], q: { x: [false, true], y: null }}|}
];

let example =
  VJson.(
    parseInCurlyBraces(
      defaultParseVariable,
      "{
       \"id\": {{id}},
       \"color\": {{color}},
       \"size\": {{size}},
     }",
    )
  )
  ->Belt.Result.getExn;

module FindVariablesTests = {
  describe("findVariables", () => {
    test("it finds variables", () => {
      expect(example->findVariables)->toEqual([|"id", "color", "size"|])
    })
  });
};

module SerializeTests = {
  let expectSerialize = (vj: vjson(string), str) =>
    expect(vj |> VJson.serializeWrapCurlyBraces(s => s))->toEqual(str);
  test("simple values", () => {
    expectSerialize(Bool(true), "true");
    expectSerialize(String("hello"), "\"hello\"");
    expectSerialize(Variable("varvara"), "{{varvara}}");
  });

  test("complex json", () =>
    expect(exampleJson->VJson.Builder.json->VJson.serialize(s => s))
    ->toMatchSnapshot()
  );

  test("complex with variables", () => {
    let vj =
      VJson.Builder.(
        object_([|
          ("x", float(1.0)),
          ("y", int(2)),
          (
            "z",
            vjsonArray([|float(1.0), number(2.0), variable("vivvy")|]),
          ),
          (
            "q",
            object_([|
              ("x", Array([|Bool(false), "oogabooga"->variable|])),
              ("y", variable("yo")),
            |]),
          ),
        |])
      );
    expect(vj->VJson.serialize(s => s))->toMatchSnapshot();
  });
};

module FromJsonTests = {
  describe("fromJson", () => {
    test("nested object", () =>
      expect(exampleJson->VJson.Builder.json)
      ->toEqual(
          Object(
            VJson.Builder.(
              [|
                ("x", number(1.0)),
                ("y", number(2.0)),
                (
                  "z",
                  [|number(1.0), number(2.0), string("hello")|]
                  |> array(id),
                ),
                (
                  "q",
                  object_([|
                    ("x", [|false, true|] |> array(bool)),
                    ("y", null),
                  |]),
                ),
              |]
            )
            ->JsMap.fromArray,
          ),
        )
    )
  });
};

module ToJsonTests = {
  describe("toJson", () => {
    let variableToJson: string => Js.Json.t =
      v =>
        switch (v) {
        | "id" => Json.Encode.int(123)
        | "color" => Json.Encode.string("pink")
        | _ => Json.Encode.null
        };

    expect(example |> toJson(variableToJson))
    ->toEqual([%raw {|{id: 123, size: null, color: "pink"}|}]);
  });
};
