# VJson: JSON with variables

This defines a superset of JSON, with the addition of "variables" of the form `{{someVariable}}`.

```json
{
  "id": {{id}},
  "color": {{color}},
  "allSizes": ["small", "medium", "large"],
  "size": {{size}},
}
```

This library provides tools for:

* Parsing the above syntax
* Finding the set of variables in a given tree
* Replacing all of the variables in a tree with JSON, producing a JSON object

## Example

```reason
let vJson: VJson.t = VJson.parseExn("{
  \"id\": {{id}},
  \"color\": {{color}},
  \"allSizes\": [\"small\", \"medium\", \"large\"],
  \"size\": {{size}},
}");

// Prints "Set { 'id', 'color', 'size' }"
Js.log(vJson->VJson.findVariables);
```

# Build
```
yarn re:build
```

# Watch

```
yarn re:watch
```
