# VJson: JSON with variables

This defines a superset of JSON, with the addition of "variables"
which appear between curly braces (e.g. `{{someVariable}}`). The
format of the variables can be customized by the user, although by
default it will parse an alphanuxmeric identifier which can start with
a letter or underscore and subsequently contain any letter, digit or
`_`. Although by default variables are strings, the `vjson` type is
polymorphic and the structure can be mapped over to support more
complex information.

```json
{
  "id": {{id}},
  "color": {{color}},
  "allSizes": ["small", "medium", "large"],
  "size": {{size}},
}
```

This library provides tools for:

* Parsing the above syntax, including custom variable formats
* Finding the set of variables in a given tree
* Replacing all of the variables in a tree with JSON, producing a JSON object

## Example

```reason
let vJson: VJson.t(string) = VJson.parseDefault("{
  \"id\": {{id}},
  \"color\": {{color}},
  \"allSizes\": [\"small\", \"medium\", \"large\"],
  \"size\": {{size}},
}")->Belt.Result.getExn;

// Prints "Set { 'id', 'color', 'size' }"
Js.log(vJson->VJson.findVariables);

// Customizing the variable format to integers
let vJsonWithInts: VJson.t(int) = VJson.(
    "{{123}}"
    |> VJson.parseWithRegex([%re {|/\d+/|}])
    |> Belt.Result.getExn
  )
  ->VJson.map(int_of_string)
```

# Build
```
yarn re:build
```

# Watch

```
yarn re:watch
```
