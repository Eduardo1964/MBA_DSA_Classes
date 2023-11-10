
## Análise de Clusters
## Fonte: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 09

# Curso: MBA DSA USP ESALQ

# Prof. Wilson Tarantin Jr.

# Instalação e carregamento dos pacotes utilizados

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
                        #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregamento da base de dados (Vestibular)
load(file = "Vestibular.RData")

# Visualização da base de dados
Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Gráfico 3D com scatter
rownames(Vestibular) <- Vestibular$estudante

scatter3D(x=Vestibular$fisica,
          y=Vestibular$matematica,
          z=Vestibular$quimica,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Fisica",
          ylab = "Matematica",
          zlab = "Quimica",
          main = "Vestibular",
          clab = "Nota de Quimica")>
  text3D(x=Vestibular$fisica,
         y=Vestibular$matematica,
         z=Vestibular$quimica,
         labels = rownames(Vestibular),
         add = TRUE, cex = 1)

# Estatísticas descritivas
summary(Vestibular)
## Como as variáveis estão na mesma unidade de medida, não vamos padronizar

# Se for necessário padronizar, é possível utilizar a função scale()
vest_padronizado <- as.data.frame(scale(Vestibular[,2:4]))
rownames(vest_padronizado) <- Vestibular$estudante

# Boxplots por variável
ggplotly(
  Vestibular %>%
    melt() %>%
    ggplot(aes(label = estudante)) +
    geom_boxplot(aes(x = variable, y = value, fill = variable)) +
    geom_point(aes(x = variable, y = value), alpha = 0.5) +
    labs(x = "Variável",
         y = "Nota") +
    scale_fill_manual("Legenda:",
                      values = c("orange", "purple", "bisque4")) +
    theme_bw()
)

#---------- Esquema de aglomeração hierárquico ---------------------------------

# Matriz de dissimilaridades
matriz_D <- Vestibular %>% 
  select(matematica, fisica, quimica) %>% 
  dist(method = "euclidean")

# Method: parametrização da distância a ser utilizada

## "euclidean": distância euclidiana
## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
## "maximum": distância de Chebychev;
## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
## "canberra": distância de Canberra;
## "minkowski": distância de Minkowski

# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = matriz_D, method = "single")

# O input é a matriz de distâncias obtida anteriormente

# Method é o tipo de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

# Definição do esquema hierárquico de aglomeração

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes

# Tabela com o esquema de aglomeração. Interpretação do output:

## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio

esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier)

# Dendrograma com visualização dos clusters (definição de 3 clusters)
fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "deeppink"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
Vestibular$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

# Visualização da base de dados com a alocação das observações nos clusters
Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Estatísticas descritivas dos clusters por variável
## ATENÇÃO: Clusters 2 e 3 têm somente uma observação, não calcula 'sd'

# Estatísticas descritivas da variável 'matematica'
group_by(Vestibular, cluster_H) %>%
  summarise(
    mean = mean(matematica, na.rm = TRUE),
    sd = sd(matematica, na.rm = TRUE),
    min = min(matematica, na.rm = TRUE),
    max = max(matematica, na.rm = TRUE))

# Estatísticas descritivas da variável 'fisica'
group_by(Vestibular, cluster_H) %>%
  summarise(
    mean = mean(fisica, na.rm = TRUE),
    sd = sd(fisica, na.rm = TRUE),
    min = min(fisica, na.rm = TRUE),
    max = max(fisica, na.rm = TRUE))

# Estatísticas descritivas da variável 'quimica'
group_by(Vestibular, cluster_H) %>%
  summarise(
    mean = mean(quimica, na.rm = TRUE),
    sd = sd(quimica, na.rm = TRUE),
    min = min(quimica, na.rm = TRUE),
    max = max(quimica, na.rm = TRUE))

# Análise de variância de um fator (ANOVA). Interpretação do output:

## Mean Sq do cluster_H: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_H / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F (e significativa)

# ANOVA da variável 'matematica'
summary(anova_matematica <- aov(formula = matematica ~ cluster_H,
                                data = Vestibular))

# ANOVA da variável 'fisica'
summary(anova_fisica <- aov(formula = fisica ~ cluster_H,
                            data = Vestibular))

# ANOVA da variável 'quimica'
summary(anova_quimica <- aov(formula = quimica ~ cluster_H,
                             data = Vestibular))

#---------- Esquema de aglomeração não hierárquico K-MEANS ---------------------

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(Vestibular[,2:4],
                         centers = 3)

## centers: parametrização da quantidade de clusters

# Método de Elbow para identificação do número ótimo de clusters
## Apresenta a variação total dentro dos clusters para várias nº de clusters
## Em geral, quando há a dobra é um indício do número ótimo de clusters
fviz_nbclust(Vestibular[,2:4], kmeans, method = "wss", k.max = 4)

# Criando variável categórica para indicação do cluster no banco de dados
Vestibular$cluster_K <- factor(cluster_kmeans$cluster)

# Visualização da base de dados
Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Análise de variância de um fator (ANOVA)

# ANOVA da variável 'matematica'
summary(anova_matematica <- aov(formula = matematica ~ cluster_K,
                                data = Vestibular))

# ANOVA da variável 'fisica'
summary(anova_fisica <- aov(formula = fisica ~ cluster_K,
                            data = Vestibular))

# ANOVA da variável 'quimica'
summary(anova_quimica <- aov(formula = quimica ~ cluster_K,
                             data = Vestibular))

# Comparando os resultados dos esquemas hierárquico e não hierárquico
Vestibular %>%
  select(estudante, cluster_H, cluster_K) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# FIM!