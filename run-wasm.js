const fs = require("fs");
const path = require("path");
let [_x, _y, file, fn, ...args] = process.argv;

if (fs.existsSync(file)) {
  WebAssembly.instantiate(fs.readFileSync(file))
    .then((module) => {
      console.log(module.instance.exports);
      if (fn) {
        console.log(module.instance.exports[fn](...args));
      }
    })
    .catch(console.error);
} else {
  console.error("cannot find file");
}
