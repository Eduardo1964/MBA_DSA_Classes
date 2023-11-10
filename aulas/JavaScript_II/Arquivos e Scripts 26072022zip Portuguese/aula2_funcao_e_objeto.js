// Função - https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Functions
/*
Trecho de código que pode ser reutilizado.
Altamente importante para padronizar e manter regras de negócio no mesmo lugar.
    - Precisa de um nome
    - Pode receber parâmetros
    - Pode retornar
*/

// Exemplo 1: Array de silgas de cidades
var siglas = ["bh", "sp", "poa"];

// No exemplo, definimos que toda sigla deve ser tranformada em maiúsculo em nosso sistema. 
// Para isso usamos a função toUpperCase().
console.log(siglas[0].toUpperCase()); // exibindo a primeira sigla em maiúsculo
console.log(siglas[1].toUpperCase()); // exibindo a segunda sigla em maiúsculo
console.log(siglas[2].toUpperCase()); // exibindo a terceira sigla em maiúsculo

// O problema de fazer da forma acima é que a regra definida (a sigla PRECISA ser em maiúscula) 
// vai ser replicada em todo o código, ou todo o sistema. Isso é ruim em uma possível
// manutenção ou até mesmo em uma mudança da regra.
// Para resolver isso a gente cria uma função!

function acertarSigla(sigla){
    // Ao criar a função tinhamos somente uma regra: Siglas em maiúsculo
    var sigla_final = sigla.toUpperCase();

    // Ao final da explicação surgiu um "problema": Siglas sendo recebidas com - e a UF.
    // Para resolver isso usamos:
    //     1) comando split() que separa uma string(texto) em um array
    //     2) usando um separador "-" como referência
    //     3) Então a variavel sigla_final vai deixar de ser "BH-MG"  e virar um array dessa forma ["BH", "MG"]
    sigla_final = sigla_final.split("-");
    
    // vamos retornar o primeiro elemento do array, ou seja, vai ignorar a UF.
    return sigla_final[0]
}

// Precisamos garantir que o que já funcionava continua funcionando. Então vamos chamar a função
// para as mesmas siglas que foram definidas la no inicio do programa.
console.log(acertarSigla(siglas[0]));
console.log(acertarSigla(siglas[1]));
console.log(acertarSigla(siglas[2]));

// Agora vamos verificar se a modificação que foi feita para ignorar a UF também funcionou.
console.log(acertarSigla("bh-mg"));

// Vimos sobre funções anônimas. Elas não precisam de nome e 
// também são conhecidas como callbacks.
// A função setTimeout(funcao, tempo) executa o trecho/função de código depois de um determinado tempo,
// que deve ser definido em mili-segundos.
setTimeout(function(){
    console.log("Esse print demorou 5s para aparecer.");
}, 5000);
console.log("Esse print declarado depois do setTimeout() vai aparecer antes.");

// Objetos - https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Working_with_Objects
/*
Objeto é um grupo de propriedades/variáveis, que tem relação direta com o objeto.
*/

// Definimos uma variável do tipo object. O conteúdo deve estar dentro de chaves '{ }'
// O objeto armazena sempre no formato chave: valor:
//    - Chave: Pode ser uma string ou até mesmo uma função.
//    - Valor: Pode ser qualquer tipo de dado, um texto, um número, um array ou até mesmo um objeto.
// Exemplo: var objeto = {"nome_do_curso": "DSA"}
// Para separar um grupo de chave+valor utilizamos a vírgula.
// Esse tipo de dado é tão generalista que consegue representar qualquer 'coisa'. Como a leitura
// é simples, ele foi utilizado para criação do formato JSON (JavaSript Object Notation - https://pt.wikipedia.org/wiki/JSON)
// O JSON é a representação de um objeto e é utilizado para transferir dados de um sistema para outro, não importando
// em qual linguagem de programação eles foram codificados.
var pessoa = {
    "nome": "Marcelo Sabadini", 
    // Olha isso. É uma propriedade do objeto armazendando uma função. 
    // Quando invocada ela retorna a quantidade de filhos, ou seja, a quantidade de objetos dentro do array filhos.
    "totalFilhos": function(){
        return this.filhos.length
    },
    "idade": 37,
    "instrumentos": ["Guitarra", "Bateria"],
    "filhos": [
        {
            "nome": "Israel", 
            "idade": 8,
        },
        {
            "nome": "Giovana", 
            "idade": 6,
        },
    ]
}

// Vamos dar uma espiada no objeto
console.log(pessoa);

// Vamos invocar a propriedade/função totalFilhos()
console.log("Total de filhos:", pessoa.totalFilhos());


