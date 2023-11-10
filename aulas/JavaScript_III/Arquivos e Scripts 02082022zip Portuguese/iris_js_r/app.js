//

// Incluir o caminho do R no path do WINDOWS
// set PATH=%PATH%;"C:\Program Files\R\R-4.1.3\bin"

//

// Inclui os módulos necessários
const express = require('express')
const { classifica } = require("./predict")

// Instancia uma aplicação do express
const app = express()

// Adiciona o plugin para lidar com requisições em formato JSON
app.use(express.json())

// Cria uma rota do tipo post para a predição dos dados
app.post('/predict', classifica)

app.listen(9000)