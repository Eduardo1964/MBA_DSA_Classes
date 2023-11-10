const R = require("r-script")

var out = R("ola_mundo/ola_mundo.R")
  .data("Olá mundo de dentro do R!!!", 20)
  .callSync();

console.log(out)