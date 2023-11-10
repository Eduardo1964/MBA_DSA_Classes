################################################################################
###      AULA ANALYTICS PARA AVALIAÇÃO E ANÁLISE DE RISCOS - Prof. Fabiano   ###
################################################################################

# Carregando Pacotes  para Gestão de Carteiras - Teoria de Markowitz


#Pacotes utilizados
pacotes <- c("quantmod","PerformanceAnalytics","magrittr","ggplot2",
             "fPortfolio","timeSeries","dplyr","yfR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Dados Iniciais

data_inicial=Sys.Date()-120
data_final=Sys.Date()
 




###############################################################################
# As ações escolhidas a seguir NÃO SÃO RECOMENDAÇÕES DE INVESTIMENTOS POR NOSSA 
# PARTE, refletem apenas exemplos meramente ilustrativos da prática.
###############################################################################




acao='PETR4.SA'

# Rotina de captura dos preços dos ativos 

buscar=yf_get(tickers = acao, 
                first_date = data_inicial,
                last_date = data_final)

# ver a saída da busca

buscar


# pegar apenas as cotações de fechamento

petr4=buscar$ret_closing_prices

petr4=petr4[-1]
petr4 

# Apenas algumas estatísticas para ilustrar

hist(petr4)
risco_petr4=sd(petr4)
risco_petr4



# Buscar outro ativo

acao='VALE3.SA'

# Rotina de captura dos preços dos ativos 

buscar=yf_get(tickers = acao, 
              first_date = data_inicial,
              last_date = data_final)

vale3=buscar$ret_closing_prices
vale3=vale3[-1]
vale3

acao='EMBR3.SA'

# Rotina de captura dos preços dos ativos 

buscar=yf_get(tickers = acao, 
              first_date = data_inicial,
              last_date = data_final)

embr3=buscar$ret_closing_prices
embr3=embr3[-1]
embr3

# Montar a Matriz de Retornos

retornos=cbind(petr4,vale3,embr3)

retornos

dados=as.timeSeries(retornos)
dados
plot(dados)

# Cálculo dos Retornos Médios

ret.esperados=colMeans(dados)
ret.esperados
  

# Cálculo da Matriz de covariâncias

mat.cov = cov(dados)
mat.cov

x=length(petr4)
x
cov.pop <- cov(dados)*(x-1)/(x)

cov.pop


# Carteira de Minimo Risco
cart.min.risco = minvariancePortfolio(dados)
cart.min.risco

# Carteira Ótima
cart.otima = tangencyPortfolio(dados)
cart.otima

# Fronteira
fronteira = portfolioFrontier(dados)
fronteira

# Gráfico da Fronteira
frontierPlot(fronteira, col=c("blue", "Orange"), pch=19)

# traçar várias carteiras

monteCarloPoints(fronteira, mcSteps = 1000, cex=0.25, pch=19)

# Mostrar o local da carteira com proporções iguais de cada ativo
equalWeightsPoints(fronteira, pch=20, col="red")

# Mostrar os pontos relativos a cada um dos ativos escolhidos individualmente
singleAssetPoints(fronteira, pch=19, cex=1.5, col=topo.colors(6))

################## FIM #########################################################
