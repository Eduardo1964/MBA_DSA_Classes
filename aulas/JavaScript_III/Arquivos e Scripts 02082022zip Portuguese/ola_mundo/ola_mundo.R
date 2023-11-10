printa_vezes <- function(list, qtde){
    mensagem <- paste(list, qtde)
    rep(mensagem, qtde)
}

do.call(printa_vezes, input)