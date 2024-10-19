let number = %re(`/^([-+]?\d*\.?\d+)(?:[eE]([-+]?\d+))?/`)
let string = %re(`/^"((?:[^"\\]|\\.)*)"/`)
let escapedQuote = %re(`/\\"/g`)
let variable = %re(`/[a-zA-Z_][a-zA-Z0-9_]*/`)
