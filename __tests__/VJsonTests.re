open Jest;
open Expect;
open VJson;

let exampleJson: Js.Json.t = [%raw
  {|{x: 1, y: 2, z: [1, 2, "hello"], q: { x: [false, true], y: null }}|}
];

let example =
  VJsonParser.parseExn(
    "{
            \"id\": {{id}},
            \"color\": {{color}},
            \"size\": {{size}},
          }",
  );

module FindVariablesTests = {
  describe("findVariables", () => {
    test("it finds variables", () => {
      expect(example->findVariables)
      ->toEqual(
          JsSet.fromArray(
            [|"id", "color", "size"|]
            ->Belt.Array.map(VariableName.fromString),
          ),
        )
    })
  });
};

module SerializeTests = {
  let variableToString = VariableName.toString;
  let expectSerialize = (vj, str) =>
    expect(vj->toString(variableToString))->toEqual(str);
  test("simple values", () => {
    expectSerialize(Bool(true), "true");
    expectSerialize(String("hello"), "\"hello\"");
    expectSerialize(
      Variable(VariableName.fromString("varvara")),
      "{{varvara}}",
    );
  });
};

module FromJsonTests = {
  describe("fromJson", () => {
    test("nested object", () =>
      expect(exampleJson->fromJson)
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
    let variableToJson: VariableName.t => Js.Json.t =
      vn =>
        switch (vn->VariableName.toString) {
        | "id" => Json.Encode.int(123)
        | "color" => Json.Encode.string("pink")
        | _ => Json.Encode.null
        };

    expect(example |> toJson(variableToJson))
    ->toEqual([%raw {|{id: 123, size: null, color: "pink"}|}]);
  });
};
