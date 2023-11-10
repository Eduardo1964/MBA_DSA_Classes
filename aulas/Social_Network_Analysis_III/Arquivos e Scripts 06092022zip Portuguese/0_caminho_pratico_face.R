##########################
# CARREGANDO BIBLIOTECAS #
##########################

library (igraph)
library (readr)


#####################
# IMPORTANTO DADOS  #
#####################

#importe os dados já no formato de rede
dri <- read.graph("dados/ADRIANA.gml",format=c("gml"))

# bora pensar... qual sera a sua estrategia?
# vai querer fazer as comunidades antes ou depois?
# defina sua estrategia e crie conforme sua ordem 
# voce tem todos os codigos anteriores para copiar 
# e colar, abuse!
