function hello() {
    console.log("Olá mundo");
}

class Pessoa {
    constructor(nome) {
        this.nome = nome;
    }
    apresentar() {
        console.log("Olá, eu me chamo", this.nome)
    }
}


// Exportando mais de um elemento em um módulo
module.exports = {
    hello,
    Pessoa
}