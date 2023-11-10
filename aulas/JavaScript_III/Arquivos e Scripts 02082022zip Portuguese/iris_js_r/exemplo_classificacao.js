var R = require("r-script");

var flor = 
{
        sepal_length: 5.7,
        sepal_width: 2.8,
        petal_length: 4.1,
        petal_width: 1.3
};

    // Carrega o script com os dados do objeto flor
R("r_scripts/iris_predict.R").data(flor)
    .call(function(erro, data) {
        if (erro) throw erro;
        console.log(data);
    });
