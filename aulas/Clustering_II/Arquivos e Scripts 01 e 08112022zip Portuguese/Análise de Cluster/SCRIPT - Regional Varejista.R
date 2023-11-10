
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

# Carregamento da base de dados
load(file = "Regional Varejista.RData")

## Contexto: notas médias de cada uma das 18 lojas para os 3 atributos medidos
## As lojas estão divididas em 3 regionais

# Visualização da base de dados
RegionalVarejista %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Gráfico 3D com scatter
rownames(RegionalVarejista) <- RegionalVarejista$loja

scatter3D(x=RegionalVarejista$atendimento,
          y=RegionalVarejista$sortimento,
          z=RegionalVarejista$organização,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Atendimento",
          ylab = "Sortimento",
          zlab = "Organização",
          main = "Lojas",
          clab = "Notas Médias")>
  text3D(x=RegionalVarejista$atendimento,
         y=RegionalVarejista$sortimento,
         z=RegionalVarejista$organização,
         labels = rownames(RegionalVarejista),
         add = TRUE, cex = 1)

# Estatísticas descritivas
summary(RegionalVarejista$atendimento)
summary(RegionalVarejista$sortimento)
summary(RegionalVarejista$organização)

## Neste caso, não faremos a padronização. As variáveis já estão na mesma escala

#---------- Esquema de aglomeração hierárquico ---------------------------------

# Matriz de dissimilaridades
matriz_D <- RegionalVarejista %>% 
  select(atendimento, sortimento, organização) %>% 
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

# Dendrograma com visualização dos clusters
# Definindo 3 clusters para comparar com regionais
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
RegionalVarejista$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

# Visualização da base de dados com a alocação das observações nos clusters
RegionalVarejista %>%
  select(regional, cluster_H) %>% 
  arrange(regional) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Estatísticas descritivas dos clusters por variável

# Estatísticas descritivas da variável 'atendimento'
group_by(RegionalVarejista, cluster_H) %>%
  summarise(
    mean = mean(atendimento, na.rm = TRUE),
    sd = sd(atendimento, na.rm = TRUE),
    min = min(atendimento, na.rm = TRUE),
    max = max(atendimento, na.rm = TRUE))

# Estatísticas descritivas da variável 'sortimento'
group_by(RegionalVarejista, cluster_H) %>%
  summarise(
    mean = mean(sortimento, na.rm = TRUE),
    sd = sd(sortimento, na.rm = TRUE),
    min = min(sortimento, na.rm = TRUE),
    max = max(sortimento, na.rm = TRUE))

# Estatísticas descritivas da variável 'organização'
group_by(RegionalVarejista, cluster_H) %>%
  summarise(
    mean = mean(organização, na.rm = TRUE),
    sd = sd(organização, na.rm = TRUE),
    min = min(organização, na.rm = TRUE),
    max = max(organização, na.rm = TRUE))

# Análise de variância de um fator (ANOVA). Interpretação do output:

## Mean Sq do cluster_H: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_H / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F (e significativa)

# ANOVA da variável 'atendimento'
summary(anova_atendimento <- aov(formula = atendimento ~ cluster_H,
                                 data = RegionalVarejista))

# ANOVA da variável 'sortimento'
summary(anova_sortimento <- aov(formula = sortimento ~ cluster_H,
                                data = RegionalVarejista))

# ANOVA da variável 'organização'
summary(anova_organiza <- aov(formula = organização ~ cluster_H,
                              data = RegionalVarejista))

# Vamos realizar uma análise de robustez do resultado anterior
## Vamos alterar a medida de distância e o método de encadeamento

# Elaboração da matriz de distâncias com a distância de 'Manhattan'
matriz_DM <- RegionalVarejista %>% 
  select(atendimento, sortimento, organização) %>% 
  dist(method = "manhattan")

# Clusterização hierárquica com método 'complete'
cluster_hier_man <- agnes(x = matriz_DM, method = "complete")

# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier_man)

## De fato, o resultado também aponta para 3 clusters

# Definindo 3 clusters para comparar com regionais
fviz_dend(x = cluster_hier_man,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "deeppink"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

# Adicionando a variável ao banco de dados
RegionalVarejista$cluster_H_man <- factor(cutree(tree = cluster_hier_man, k = 3))

## Para este caso, gera exatamente o mesmo resultado

#---------- Esquema de aglomeração não hierárquico K-MEANS ---------------------

# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(RegionalVarejista[,3:5],
                         centers = 3)

# Criando variável categórica para indicação do cluster no banco de dados
RegionalVarejista$cluster_K <- factor(cluster_kmeans$cluster)

# Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(RegionalVarejista[,3:5], kmeans, method = "wss", k.max = 10)

# Visualização da base de dados
RegionalVarejista %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Análise de variância de um fator (ANOVA)

# ANOVA da variável 'atendimento'
summary(anova_atendimento <- aov(formula = atendimento ~ cluster_K,
                                 data = RegionalVarejista))

# ANOVA da variável 'sortimento'
summary(anova_sortimento <- aov(formula = sortimento ~ cluster_K,
                                data = RegionalVarejista))

# ANOVA da variável 'organização'
summary(anova_organiza <- aov(formula = organização ~ cluster_K,
                              data = RegionalVarejista))

# Comparando os resultados dos esquemas hierárquico e não hierárquico
RegionalVarejista %>%
  select(regional, cluster_H, cluster_K) %>%
  arrange(regional) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# FIM!