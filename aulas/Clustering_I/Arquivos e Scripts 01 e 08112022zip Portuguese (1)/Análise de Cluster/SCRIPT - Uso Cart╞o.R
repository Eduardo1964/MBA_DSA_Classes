
## Análise de Cluster

# Curso: MBA DSA USP ESALQ

# Prof. Wilson Tarantin Jr.

# Objetivo: categorizar os clientes de uma operadora de cartão de crédito
# Analisar os grupos de clientes mais e menos leais à marca (por meio do uso)
# Fonte: https://www.kaggle.com/datasets/aryashah2k/credit-card-customer-data

# Carregando e instalando os pacotes

pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel", 
             "knitr", 
             "kableExtra",
             "reshape2",
             "misc3d",
             "plot3D", 
             "cluster", 
             "factoextra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando o dataset

dados_uso <- read.csv("cartao_credito.csv")

# Realizando a padronização dos dados

dados_padronizado <- as.data.frame(scale(dados_uso[,3:7]))

# Visualização

scatter3D(x=dados_padronizado$Avg_Credit_Limit,
          y=dados_padronizado$Total_Credit_Cards,
          z=dados_padronizado$Total_visits_bank,
          phi = 1, bty = "g", pch = 20, cex = 1,
          xlab = "Limite Médio",
          ylab = "Nº Cartões",
          zlab = "Nº Visitas",
          main = "Clientes", 
          colkey = F)

# Método de Elbow para identificação do número ótimo de clusters
dev.off()
fviz_nbclust(dados_padronizado, kmeans, method = "wss", k.max = 10)

## Podemos concluir que 4 clusters é uma opção viável

# Elaboração da clusterização não hieráquica k-means

cluster_kmeans <- kmeans(dados_padronizado,
                         centers = 4)

# Adicionando a variável

dados_padronizado$cluster_K <- factor(cluster_kmeans$cluster)
dados_uso$cluster_K <- factor(cluster_kmeans$cluster)

# Analisando por meio de gráficos 

ggplot(dados_padronizado) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_Credit_Cards, 
                 color = cluster_K)) + 
  labs(x = "Limite Médio",
       y = "Quantidade de Cartões")


ggplot(dados_padronizado) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_visits_bank, 
                 color = cluster_K)) + 
  labs(x = "Limite Médio",
       y = "Quantidade de Visitas ao Banco")


ggplot(dados_padronizado) +
  geom_point(aes(x = Avg_Credit_Limit, 
                 y = Total_visits_online, 
                 color = cluster_K)) + 
  labs(x = "Limite Médio",
       y = "Quantidade de Visitas ao Banco")


# Analisando por meio de estatísticas descritivas

análise <- group_by(dados_uso, cluster_K) %>%
  summarise(limite = mean(Avg_Credit_Limit, na.rm = TRUE),
            q_cartoes = mean(Total_Credit_Cards, na.rm = TRUE),
            q_vistias = mean(Total_visits_bank, na.rm = TRUE),
            q_online = mean(Total_visits_online, na.rm = TRUE),
            q_liga = mean(Total_calls_made, na.rm = TRUE))

# Anovas

summary(anova_limite <- aov(formula = Avg_Credit_Limit ~ cluster_K,
                            data = dados_padronizado))

summary(anova_cartoes <- aov(formula = Total_Credit_Cards ~ cluster_K,
                             data = dados_padronizado))

summary(anova_visitas <- aov(formula = Total_visits_bank ~ cluster_K,
                             data = dados_padronizado))

summary(anova_online <- aov(formula = Total_visits_online ~ cluster_K,
                            data = dados_padronizado))

summary(anova_liga <- aov(formula = Total_calls_made ~ cluster_K,
                          data = dados_padronizado))

## Todas as variáveis são relevantes na criação de pelo menos um cluster

# Fim!