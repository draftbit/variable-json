// Make alphanumeric equivalents of the operators in Bastet which are really hard to read in rescript syntax
open ReludeParse.Parser

// Given two parsers, try the first one, if it fails try the second one
let or_ = \"<|>"

let maybe = \"<?>"

// Given two parsers, run both, but only return the left value
let left = \"<*"

// Given two parsers, run both, but only return the right value
let right = \"*>"

// Given a parser that returns a value, and a parser that takes a value as an argument,
// run the first parser and pass its result to the second
let flatMap = \">>="
