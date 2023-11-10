# Carrega a biblioteca de forma compatível com o módulo JS
needs("caret")

# Adiciona o input à busca do R.
# Ao avaliar uma variável, o R procura no objeto passado ao attach
attach(input[[1]])

# Carrega o modelo a partir de um binário (modelo treinado previamente)
my_model <- readRDS("modelo/model.rds")

# Cria um data frame com os valores passados pelo javascript
dados <- data.frame(sepal_length, sepal_width, petal_length, petal_width)

# Renomeia as variáveis do dataframe para combinar com o dataframe
# esperado pelo modelo (convenção do cientista de dados)
names(dados) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Retorna a classificação encontrada pelo modelo
predict(my_model, dados)