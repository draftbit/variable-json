open Jest;
open Expect;
open VJson;

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

module FromJsonTests = {
  describe("fromJson", () => {
    test("nested object", () =>
      expect(
        [%raw
          {|{x: 1, y: 2, z: [1, 2, "hello"], q: { x: [false, true], y: null }}|}
        ]
        ->fromJson,
      )
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
                  ->Js.Dict.fromArray,
                ),
              ),
            |]
            ->Js.Dict.fromArray,
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
