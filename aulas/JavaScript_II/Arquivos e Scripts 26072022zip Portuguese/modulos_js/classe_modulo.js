class Pessoa {
    constructor(nome) {
        this.nome = nome;
    }
    apresentar() {
        console.log("Olá, eu me chamo", this.nome)
    }
}

// Exportando um objeto que contem um elemento (no caso, a classe)
module.exports = { Pessoa };