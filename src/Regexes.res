// These mess up the emacs syntax highlighter so I put them in their own module for clarity

// Note: regex taken from https://rgxdb.com/r/1RSPF8MG, but the '$'
// at the end has been omitted because it causes parse errors
let floatRegex: Js.Re.t = %re(`/^([-+]?\d*\.?\d+)(?:[eE]([-+]?\d+))?/`)

let escapedQuoteRegex = %re(`/\\\"/gm`)
let nonEscapedQuoteRegex = %re(`/((?:^|[^\\])(?:\\{2})*)"/gm`)
let inQuotesRegex = %re(`/"(?:[^"\\]|\\.)*"/`)
