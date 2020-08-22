open Jest;
open Expect;
open VJson;
open VJsonParser;

let expectParse = (input, handler) => {
  //  let x: result('a, ReludeParse.Parser.error) = input->parse;
  switch (input->parse, handler) {
  | (Ok(vj), Ok(checks)) => vj->checks
  | (Error(err), Error(checks)) => err->checks
  | (Error(ReludeParse.Parser.ParseError.ParseError(message)), Ok(_)) =>
    failwith("Expected an Ok result, but got error: " ++ message)
  | (Ok(vj), Error(_)) =>
    failwith(
      "Expected an Error result, but successfully parsed: "
      ++ Json.stringify(Obj.magic(vj)),
    )
  };
};

// Checks that parsing is successful and the output matches the expected.
let expectOkParse = (input, output) =>
  input->expectParse(Ok(vj => expect(vj)->toEqual(output)));

// Given checks to run on the error message, checks that parse fails in
// the given manner.
let expectParseFail = (input, errMessageChecks) =>
  input->expectParse(Error(errMessageChecks));

// Tests failure in a generic way, just that it's an Error of some kind.
let expectSomeParseFail = input => input->expectParseFail(_ => ());

module ParserTests = {
  describe("null", () => {
    test("single value", () =>
      expectOkParse("null", Null)
    );
    test("array", () =>
      expectOkParse("[null, null]", Array([|Null, Null|]))
    );
    test("bad array (missing closing brackets)", () =>
      expectParseFail("[ null, null", (ParseError(m)) =>
        expect(m)->toEqual(stringContaining("]"))
      )
    );
    test("bad array (no comma)", () =>
      expectSomeParseFail("[ 1 true ]")
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
    test("with preceding  whitespace", () =>
      expectOkParse("   true ", Bool(true))
    );
  });

  describe("numbers", () => {
    test("integer", () =>
      expectOkParse("123", Number(123.0))
    );
    test("float", () =>
      expectOkParse("123.456", Number(123.456))
    );
    test("negative float", () =>
      expectOkParse("-123.456", Number(-123.456))
    );
    test("float without leading digits", () =>
      expectOkParse(".456", Number(0.456))
    );
    test("float in scientific notation", () =>
      expectOkParse("+1.2e5", Number(120000.0))
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
    test("unicode", () =>
      expectOkParse(
        "\"世界こんにちは！\"",
        String("世界こんにちは！"),
      )
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
