##########################
# CARREGANDO BIBLIOTECAS #
##########################

library (igraph)


#########################
# NOSSO EXEMPLO NA MAO  #
#########################

# desenhando a rede do exemplo
g <- graph(edges = c("A","B",  "B","C",  "B","D",  "D","C", "C","E"), directed = F)


### conhecendo o objeto g
# verificando a classe
class(g)
# vendo o objeto
g
# repare que ele eh um objeto igraph

############################
# Entendendo objeto igraph #
############################

# Interpretando o objeto
#' A descricao de um objeto igraph comeca com ate quatro letras:
#' D para um grafico direcionado 
#' U para um grafico nao direcionado
#' N para um grafico nomeado (onde os nos tem um atributo de nome)
#' W para um grafico ponderado (em que as arestas tem um atributo de peso)
#' B para um grafico bipartido (dois modos) (onde os nos tem um atributo de tipo)
#' Os dois numeros a seguir (7 5) se referem ao numero de nos e links no grafico. 
#' 
#' A descricao tambem lista os atributos de no e borda, por exemplo:
#'   (g / c) - atributo caracter do grafo
#'   (v / c) - atributo caracter do link
#'   (e / n) - atributo numerico do no
#'   (e / c) - atributo caracter do no
#'   ou seja g - grafo / v - no (vertice) / e - link (edge) / c - caracter / n - numerico
