# SCRIPT DA DEFINIÇÃO DO SERVIDOR DA API

# instalação do pacote contendo as funções de criação de servidor da API
install.packages("plumber")

# adicionando a rota do script api-rotas.R na API
p <- plumber::plumb("./api-rotas.R")

# O SERVIÇO DA API SERÁ INICIADA NA PORTA 8000 
p$run(host = "0.0.0.0", port = 8000)
