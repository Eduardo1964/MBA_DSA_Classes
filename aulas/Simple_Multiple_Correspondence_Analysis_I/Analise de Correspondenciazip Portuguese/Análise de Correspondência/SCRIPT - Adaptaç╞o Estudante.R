
## Análise de Correspondência Múltipla

# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.

# Fonte: adaptado de https://www.kaggle.com/datasets/mdmahmudulhasansuzan/students-adaptability-level-in-online-education
# Suzan et al.(2021) Students' Adaptability Level Prediction in Online Education using Machine Learning Approaches - DOI: 10.1109/ICCCNT51525.2021.9579741

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap", 
             "ade4",
             "readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando a base de dados
dados_adapta <- read.csv("estudantes_adapta.csv")

# A função para a criação da ACM pede que sejam utilizados "fatores"
dados_adapta <- as.data.frame(unclass(dados_adapta), stringsAsFactors=TRUE)

# Frequências observadas por variável
summary(dados_adapta)

# Tabelas de contingência
sjt.xtab(var.row = dados_adapta$Adaptivity,
         var.col = dados_adapta$Education,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_adapta$Adaptivity,
         var.col = dados_adapta$Institution,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_adapta$Adaptivity,
         var.col = dados_adapta$Financial,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_adapta$Adaptivity,
         var.col = dados_adapta$Internet,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

# Criando a ACM
ACM <- dudi.acm(dados_adapta, scannf = FALSE, nf = 3)

## Foram extraídas as coordenadas para 3 dimensões: nf = 3
## O intuito é plotar um gráfico tridimensional

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(dados_adapta,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>% 
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Mapa perceptual em 3D (3 primeiras dimensões)
ACM_3D <- plot_ly()

# Adicionando as coordenadas
ACM_3D <- add_trace(p = ACM_3D,
                    x = df_ACM$CS1,
                    y = df_ACM$CS2,
                    z = df_ACM$CS3,
                    mode = "text",
                    text = rownames(df_ACM),
                    textfont = list(color = "blue"),
                    marker = list(color = "red"),
                    showlegend = FALSE)

ACM_3D

# Fim!