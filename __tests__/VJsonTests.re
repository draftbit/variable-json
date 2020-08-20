open Jest;
open Expect;
open VJson;

let example =
  parseExn(
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

module ParserTests = {
  let expectOkParse = (input, output) =>
    expect(parse(input))->toEqual(Ok(output));

  describe("null", () => {
    test("single value", () =>
      expectOkParse("null", Null)
    );
    test("array", () =>
      expectOkParse("[null, null]", Array([|Null, Null|]))
    );
  });

  describe("bools", () => {
    test("true", () =>
      expect(parse("true"))->toEqual(Ok(Bool(true)))
    );
    test("false", () =>
      expect(parse("false"))->toEqual(Ok(Bool(false)))
    );
    test("with trailing whitespace", () =>
      expect(parse("false   "))->toEqual(Ok(Bool(false)))
    );
  });

  describe("numbers", () => {
    test("integer", () =>
      expectOkParse("123", Number(123.0))
    );
    test("float", () =>
      expectOkParse("123.456", Number(123.456))
    );
    test("float without leading digits", () =>
      expectOkParse(".456", Number(0.456))
    );
    Skip.test("float without trailing digits", () =>
      expectOkParse("123.", Number(123.0))
    );
  });

  describe("strings", () => {
    test("simple", () =>
      expectOkParse("\"hello!\"", String("hello!"))
    );
    test("with internal quotes", () =>
      expectOkParse("\"hello \\\"world\\\"!\"", String("hello \"world\"!"))
    );
  });

  describe("variables", () => {
    test("simple", () =>
      expectOkParse("{{myVar}}", Variable(VariableName.fromString("myVar")))
    );
    test("single letter", () =>
      expectOkParse("{{x}}", Variable(VariableName.fromString("x")))
    );
    test("with numbers", () =>
      expectOkParse("{{x123}}", Variable(VariableName.fromString("x123")))
    );
    test("with underscore", () =>
      expectOkParse("{{_123}}", Variable(VariableName.fromString("_123")))
    );
  });

  describe("array", () => {
    test("empty", () =>
      expectOkParse("[]", Array([||]))
    );
    test("empty with whitespace", () =>
      expectOkParse("[   ]", Array([||]))
    );
    test("numbers (single)", () =>
      expectOkParse("[1]", Array([|Number(1.0)|]))
    );
    test("numbers (more)", () =>
      expectOkParse(
        "[1, 2.345, 6]",
        Array([|Number(1.0), Number(2.345), Number(6.0)|]),
      )
    );
    test("nulls", () =>
      expectOkParse("[null, null]", Array([|Null, Null|]))
    );
    test("bools", () =>
      expectOkParse(
        "[true, true, false]",
        Array([|Bool(true), Bool(true), Bool(false)|]),
      )
    );
    test("variables", () =>
      expectOkParse(
        "[{{x}}, {{coolVar}}, {{yo_123}}]",
        Array(
          [|"x", "coolVar", "yo_123"|]
          ->Belt.Array.map(s => Variable(VariableName.fromString(s))),
        ),
      )
    );
  });

  describe("object", () => {
    test("empty", () =>
      expectOkParse("{}", Object(Js.Dict.empty()))
    );
    test("empty with whitespace", () =>
      expectOkParse("{   }", Object(Js.Dict.empty()))
    );
    test("single key", () =>
      expectOkParse(
        "{\"x\": 1}",
        Object([|("x", Number(1.0))|]->Js.Dict.fromArray),
      )
    );
    test("multiple keys", () =>
      expectOkParse(
        "{\"x\": 1, \"YYY\": \"hey there!\"}",
        Object(
          [|("x", Number(1.0)), ("YYY", String("hey there!"))|]
          ->Js.Dict.fromArray,
        ),
      )
    );
    test("nested", () =>
      expectOkParse(
        "{\"x\": 1, \"YYY\": { \"z\": {{myZVariable}}, \"blib\": [123, \"hey there!\"]}}",
        Object(
          [|
            ("x", Number(1.0)),
            (
              "YYY",
              Object(
                [|
                  ("z", Variable(VariableName.fromString("myZVariable"))),
                  ("blib", Array([|Number(123.0), String("hey there!")|])),
                |]
                ->Js.Dict.fromArray,
              ),
            ),
          |]
          ->Js.Dict.fromArray,
        ),
      )
    );
  });
};
