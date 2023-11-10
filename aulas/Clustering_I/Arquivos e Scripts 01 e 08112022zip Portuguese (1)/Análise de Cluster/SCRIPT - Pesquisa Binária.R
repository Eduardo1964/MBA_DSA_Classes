
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
load(file = "Pesquisa Binária.RData")

## Contexto: são respostas binárias para 50 perguntas de 35 respondentes
## Os respondentes são gestores de empresas em 3 setores distintos

# Visualização da base de dados
View(PesquisaBinária)

# Contagem das categorias por variável
vetor_var <- names(PesquisaBinária)
map(PesquisaBinária[vetor_var], ~ summary(as.factor(.)))

#---------- Esquema de aglomeração hierárquico ---------------------------------

# Matriz de dissimilaridades
matriz_D <- PesquisaBinária %>% 
  select(-setor) %>% 
  dist.binary(method = 2)

## Em 'dist.binary', method = 2 indica similaridade por emparelhamento simples

# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = matriz_D, method = "average")

# Outras opções de encadeamento:

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
# Parametrizando 3 clusters para comparar com setores
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
PesquisaBinária$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

# Visualização da base de dados com a alocação das observações nos clusters
PesquisaBinária %>%
  select(setor, cluster_H) %>%
  arrange(setor) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# FIM!