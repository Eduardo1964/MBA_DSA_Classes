################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################

# função para instalar e carregar os pacotes necessários
instalar_carregar_pacotes <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# alterar caminho da variável de ambiente do python
Sys.setenv(MLFLOW_BIN="./venv/Scripts/mlflow")

# alterar caminho da variável de ambiente do python
Sys.setenv(MLFLOW_PYTHON_BIN="./venv/Scripts/python")

# lista de pacotes necessários
pacotes <- c("sparklyr", 
             "ggcorrplot",
             "dplyr", 
             "nlme",
             "ggplot2", 
             "carrier", 
             "mlflow", 
             "reticulate", 
             "stats", 
             "glue")

# instalar e carregar os pacotes
instalar_carregar_pacotes(pacotes)

################################################################################
#            CONECTANDO O MLFLOW PARA CONSUMIR SEUS MODELOS                    #
################################################################################

# conectando ao mlflow para gerenciar todo o ciclo de vida do modelo
mlflow_set_tracking_uri('http://localhost:5000')

################################################################################
#            CONSTRUINDO UMA API PARA CONSUMO DOS MODELOS                      #
################################################################################

# Application Programming Interface (API)

# SERVER -> ROTAS -> FUNÇÕES

# Trabalharemos com protocolos HTTP
# Protocolo de transferência que possibilidade sistemas possam se comunicar
# através de URLs e métodos

# Exemplos de URLs

# 1. http://localhost:5000
# 2. https://www.google.com/imagens
# 3. http://localhost:9000/api
# 4. https://dominio-aplicacao/api/v1/modelo/predict

# Nosso domínio da aplicação:
# http://aplicacao/api/v1

# Exemplos de métodos

# 1. GET (Retorna um recurso de dados através de inputs na URL)
# 2. POST (Retorna um recurso de dados através de dados enviados para o servidor)

# Uma rota precisa de:
# 1. O seu método HTTP
# 2. URL de acesso
# 3. Parâmetros (Caso existam)

################################################################################
#            ROTA DO PREDICT DO MODELO DESEMPENHO ESTUDANTE                    #
################################################################################

# acesso via http://aplicacao/api/v1/modelo_desempenho_estudante/predict

#* @get /modelo_desempenho_estudante/predict
#* @param texp:float
#* @param horas:int 
rota_modelo_desempenho_predict <- function(texp, horas) {
  
  # coletando o modelo em produção no mlflow
  modelo_produção_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/production")
  
  # criando um dataframe para ser consumido pelo modelo com os parâmetros
  # da rota
  novo_dado <- data.frame(texp = as.numeric(c(texp)),
                          horas = as.numeric(c(horas)))
  
  resultado <-  as.vector(modelo_produção_predict(novo_dado))
  
  paste("O desempenho previsto para esse estudante é de: ", resultado[1])
  
}

################################################################################
#            ROTA DO PREDICT DO MODELO PREVISAO DOENÇA                         #
################################################################################

# acesso via http://aplicacao/api/v1/modelo_previsao_doenca/predict

#* @get /modelo_previsao_doenca/predict
#* @param male:int
#* @param age:int 
#* @param cigsPerDay:int 
rota_modelo_previsao_doenca_predict <- function(male, age, cigsPerDay) {
  
  # coletando o modelo em produção no mlflow
  modelo_produção_predict <- mlflow_load_model("models:/modelo_previsao_doenca/production")
  
  # criando um dataframe para ser consumido pelo modelo com os parâmetros
  # da rota
  novo_dado <- data.frame(male = as.factor(c(male)),
                          age = as.numeric(c(age)),
                          cigsPerDay = as.numeric(c(cigsPerDay)))
  
  resultado <-  as.vector(modelo_produção_predict(novo_dado))
  
  paste("A probabilidade desse indivíduo ter uma doença do coração em 10 anos, é de: ", resultado[1])
  
}
