// Importando apenas um elemento do módulo
 const { hello } = require("./hello_world");
const { Pessoa } = require("./classe_modulo")

// Importando vários elementos de uma vez          
// const { hello, Pessoa } = require("./classe_hello_modulo")

// Recebendo um elemento como resultado da importação
// const hello = require("./hello_modulo")

// const { Pessoa } = require("./classe_hello_modulo")
hello()

var alexandre = new Pessoa("Alexandre");

alexandre.apresentar()