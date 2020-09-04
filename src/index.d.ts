declare module "@draftbit/variable-json" {
  interface Vjson<T> {}

  function serialize<T>(v: Vjson<T>, mapper: (t: T) => string): string;
  function parseDefaultExn(s: string): Vjson<string>;
}
