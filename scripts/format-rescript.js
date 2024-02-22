/* eslint-disable no-console */
const { basename } = require("path");
const { readFileSync, writeFileSync, lstatSync } = require("fs");
const { execSync } = require("child_process");
const fileNames = process.argv.slice(2);

if (!fileNames.length) {
  throw new Error(`Usage: ${basename(__filename)} <fileName> [<fileName>...]`);
}

let hasError = false;

fileNames.forEach(fileName => {
  if (!fileName.endsWith(".res")) {
    throw new Error(`Not a rescript file: ${fileName}`);
  }

  if (lstatSync(fileName).isSymbolicLink()) {
    console.log(`Ignoring symlink ${fileName}`);
    return;
  }

  const contents = readFileSync(fileName).toString();

  try {
    const formattedCode = execSync(`node_modules/.bin/bsc -format ${fileName}`);
    if (formattedCode !== contents) {
      writeFileSync(fileName, formattedCode);
      console.log("Formatted", fileName);
    }
  } catch {
    hasError = true;
  }
});

if (hasError) {
  process.exit(1);
}
