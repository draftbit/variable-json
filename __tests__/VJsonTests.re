open Jest;
open Expect;
open VJsonTypes;

let config = VJsonVariable.regexConfig([%re {|/[a-zA-Z_][a-zA-Z0-9_]*/|}]);

let exampleJson: Js.Json.t = [%raw
  {|{x: 1, y: 2, z: [1, 2, "hello"], q: { x: [false, true], y: null }}|}
];

let example =
  VJsonVariable.parseWithConfig(
    config,
    "{
            \"id\": {{id}},
            \"color\": {{color}},
            \"size\": {{size}},
          }",
  )
  ->Belt.Result.getExn;

module FindVariablesTests = {
  describe("findVariables", () => {
    test("it finds variables", () => {
      expect(example->VJsonOperations.findVariables)
      ->toEqual([|"id", "color", "size"|])
    })
  });
};

module SerializeTests = {
  let expectSerialize = (vj: vjson(string), str) =>
    expect(config->VJsonVariable.toString(vj))->toEqual(str);
  test("simple values", () => {
    expectSerialize(Bool(true), "true");
    expectSerialize(String("hello"), "\"hello\"");
    expectSerialize(Variable("varvara"), "{{varvara}}");
  });

  test("complex json", () =>
    expect(exampleJson->VJsonBuilder.json->VJsonOperations.serialize(s => s))
    ->toMatchSnapshot()
  );

  test("complex with variables", () => {
    let vj =
      VJsonBuilder.(
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
    expect(vj->VJsonOperations.serialize(s => s))->toMatchSnapshot();
  });
};

module FromJsonTests = {
  describe("fromJson", () => {
    test("nested object", () =>
      expect(exampleJson->VJsonBuilder.json)
      ->toEqual(
          Object(
            [|
              ("x", Number(1.0)),
              ("y", Number(2.0)),
              ("z", Array([|Number(1.0), Number(2.0), String("hello")|])),
              (
                "q",
                Object(
                  [|
                    ("x", Array([|Bool(false), Bool(true)|])),
                    ("y", Null),
                  |]
                  ->JsMap.fromArray,
                ),
              ),
            |]
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
