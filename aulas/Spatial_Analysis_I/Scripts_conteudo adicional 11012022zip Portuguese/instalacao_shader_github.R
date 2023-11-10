# Caso o objeto 3d não seja renderizado (mostrando uma figura preta),
# será necessário remover alguns pacotes e instalar a versão diretamente do github: 
remove.packages('rgl')
remove.packages('rayshader')

install.packages('cli')
install.packages('devtools')
library('devtools')
install_github('tylermorganwall/rayshader')
