open Jest
open Expect
open VJsonTypes

let expectParse = (input, handler) =>
  switch (input->VJson.parse, handler) {
  | (Ok(vj), Ok(checks)) => vj->checks
  | (Error(err), Error(checks)) => err->checks
  | (Error(e), Ok(_)) => failwith("Expected an Ok result, but got error: " ++ e)
  | (Ok(vj), Error(_)) =>
    failwith(
      "Expected an Error result, but successfully parsed: " ++ Js.Json.stringify(Obj.magic(vj)),
    )
  }

// Checks that parsing is successful and the output matches the expected.
let expectOkParse = (input, output) => input->expectParse(Ok(vj => expect(vj)->toEqual(output)))

// Given checks to run on the error message, checks that parse fails in
// the given manner.
let expectParseFail = (input, errMessageChecks) => input->expectParse(Error(errMessageChecks))

// Tests failure in a generic way, just that it's an Error of some kind.
let expectSomeParseFail = input => input->expectParseFail(e => Js.log2("Some error", e))

module ParserTests = {
  describe("null", () => {
    test("single value", () => expectOkParse("null", Null))
    test("array", () => expectOkParse("[null, null]", Array([Null, Null])))
    test("bad array (missing closing brackets)", () =>
      expectParseFail("[ null, null", m => expect(m)->toEqual(stringContaining("]")))
    )
    test("bad array (no comma)", () =>
      expectParseFail("[ 1 true ]", m => expect(m)->toEqual(stringContaining("true ]")))
    )
  })

  describe("known failure cases", () => {
    test("unmatched curly braces on a variable", () => {
      let rawText = `{
        "fields": {
          "Name": {{name}},
          "Link": {{linkUrl},
          "Monthly Rent": {{monthlyRent}},
          "Square Feet": {{squareFeet}},
          "My Notes": {{notes}}
        }
      }`
      expectParseFail(rawText, m => expect(m)->toEqual(stringContaining("}}")))
    })
  })

  describe("bools", () => {
    test("true", () => expectOkParse("true", Bool(true)))
    test("false", () => expectOkParse("false", Bool(false)))
    test("with trailing whitespace", () => expectOkParse("false   ", Bool(false)))
    test("with preceding  whitespace", () => expectOkParse("   true ", Bool(true)))
  })

  describe("numbers", () => {
    test("integer", () => expectOkParse("123", Number(123.0)))
    test("float", () => expectOkParse("123.456", Number(123.456)))
    test("negative float", () => expectOkParse("-123.456", Number(-123.456)))
    test("float without leading digits", () => expectOkParse(".456", Number(0.456)))
    test("float in scientific notation", () => expectOkParse("+1.2e5", Number(120000.0)))
    Skip.test("float without trailing digits", () => expectOkParse("123.", Number(123.0)))
  })

  describe("strings", () => {
    test("simple", () => expectOkParse("\"hello!\"", String("hello!")))
    test("with internal quotes", () =>
      expectOkParse("\"hello \\\"world\\\"!\"", String("hello \"world\"!"))
    )
    test("unicode", () =>
      expectOkParse("\"世界こんにちは！\"", String("世界こんにちは！"))
    )
    test("Nested string", () => {
      expectOkParse(
        "{\"query\": \"query foo() {bar({attribute: \\\"categoryId\\\"})}\"}",
        Object(
          Js.Dict.fromArray([("query", String("query foo() {bar({attribute: \"categoryId\"})}"))]),
        ),
      )
    })
  })

  describe("variables", () => {
    test("simple", () => expectOkParse("{{myVar}}", Variable("myVar")))
    test("single letter", () => expectOkParse("{{x}}", Variable("x")))
    test("with numbers", () => expectOkParse("{{x123}}", Variable("x123")))
    test("with underscore", () => expectOkParse("{{_123}}", Variable("_123")))
  })

  describe("array", () => {
    test("empty", () => expectOkParse("[]", Array([])))
    test("empty with whitespace", () => expectOkParse("[   ]", Array([])))
    test("numbers (single)", () => expectOkParse("[1]", Array([Number(1.0)])))
    test("numbers (more)", () =>
      expectOkParse("[1, 2.345, 6]", Array([Number(1.0), Number(2.345), Number(6.0)]))
    )
    test("nulls", () => expectOkParse("[null, null]", Array([Null, Null])))
    test("bools", () =>
      expectOkParse("[true, true, false]", Array([Bool(true), Bool(true), Bool(false)]))
    )
    test("variables", () =>
      expectOkParse(
        "[{{x}}, {{coolVar}}, {{yo_123}}]",
        Array(["x", "coolVar", "yo_123"]->Belt.Array.map(s => VJsonTypes.Variable(s))),
      )
    )
    test("trailing whitespace", () => expectOkParse("[null, null]   ", Array([Null, Null])))
  })

  describe("object", () => {
    test("empty", () => expectOkParse("{}", Object(Js.Dict.empty())))
    test("empty with whitespace", () => expectOkParse("{   }", Object(Js.Dict.empty())))
    test("single key", () =>
      expectOkParse("{\"x\": 1}", Object([("x", Number(1.0))]->Js.Dict.fromArray))
    )
    test("multiple keys", () =>
      expectOkParse(
        "{\"x\": 1, \"YYY\": \"hey there!\"}",
        Object([("x", Number(1.0)), ("YYY", String("hey there!"))]->Js.Dict.fromArray),
      )
    )
    test("extra whitespace", () =>
      expectOkParse("  {   \"x\"  : 1 }    ", Object([("x", Number(1.0))]->Js.Dict.fromArray))
    )
    test("nested", () =>
      expectOkParse(
        "{\"x\": 1, \"YYY\": { \"z\": {{myZVariable}}, \"blib\": [123, \"hey there!\"]}}",
        Object(
          [
            ("x", Number(1.0)),
            (
              "YYY",
              Object(
                [
                  ("z", Variable("myZVariable")),
                  ("blib", Array([Number(123.0), String("hey there!")])),
                ]->Js.Dict.fromArray,
              ),
            ),
          ]->Js.Dict.fromArray,
        ),
      )
    )
    test("long string value", () => {
      let longValue = "pzqtdxvnhzvmzzigsojovukwfssmzadolomslufahgjbuininzcexwrkvnncmktcqpdcsrcnerdjrwgcncswsoovrrplikznwfqbvmrkeequwohejmrdmwmfcdkkfwhmqqiqgpfpelmhutdpshonwbtkxmwfoccaqogmzulumtcywyplotpbsldrampwwtmjwsmfhnnzumyhpyforahmivaalkhrenxxvnhuwpovjnkdjbrxvhobpmffinjtuaegkqejfxfejiatxkpxvxboftretjleyxysfwlkiyjfdnvhjdtsopwuvznzpzrzvntiixdqifzsiktwhvgimwpgsgxrbczqhnycsalycgcngilyjwlxkhmaieffcpfptwzqffrlwpksmjbndftkjkjcnwqgbfwoucguujltbcfkerbwrsoeofdmmawmzlegojrujqbkftagqarwbvrlmsgwyxhpcxrdyynjrbltvrtiruhbsmroovmgqfgvogrjshjbhzgucsmavrnyuxqwaqpjhlrqargmuoixwnorvepyvtybqbjrjzyafgzwxedpyezprhtbfzrcmpysfirgemwihpzlmciehdlolhrszfqnserejqqwsazshizvyuuagitooleocilvlfmoriwzpudhqdcngayfyyptuggzloyamzxtrekqeegawxjddprqkrwepeynqifacltgbxsmpqinpaegwkuvbawuuimculerazmttvvqptbyjmnsxrpiwtkqitsoljjqgfsghckooyzdqxeagckeqmmnkpmxqsgvqvbuhlwzgumrslhyvtegfaosnbfgmqdpbkgipiequsjgyopmbiwhvuryzknwayxedhiimqqnddlzgaxbklsvwjigjqltzitphlzguzgljzvylilltqystjnbyafopkanphhuntwbffnslweajomjvgzpkcjznwpnyliymimrfvcdcbcjgwmjdpmlfjiqhizk"
      let rawText = `
      {
      "query": "${longValue}",
      "variables": {
          "searchTerm": "bag",
          "sortBy" : {{sortBy}},
        }
      }`
      expectOkParse(
        rawText,
        Object(
          [
            (
              "variables",
              Object(
                [("searchTerm", String("bag")), ("sortBy", Variable("sortBy"))]->Js.Dict.fromArray,
              ),
            ),
            ("query", String(longValue)),
          ]->Js.Dict.fromArray,
        ),
      )
    })
    test("extra line", () =>
      expectOkParse("{\n\"id\": 5}", Object([("id", Number(5.0))]->Js.Dict.fromArray))
    )
  })
}
