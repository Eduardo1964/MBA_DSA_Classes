
# Análise Fatorial por Componentes Principais (PCA)

# Curso: MBA DSA (USP ESALQ)
# Prof. Wilson Tarantin Jr.

# Pacotes necessários

pacotes <- c("tidyverse","ggrepel","reshape2","knitr","kableExtra", 
             "PerformanceAnalytics","factoextra","psych","sp","tmap")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando a base de dados
load(file = "atlasambiental.RData")

# Observando a base de dados
atlasambiental %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Estatísticas descritivas univariadas
summary(atlasambiental[,3:11])

# Estabelecendo uma matriz de correlações de Pearson
rho <- cor(atlasambiental[,3:11])

# Elaborando um mapa de calor das correlações
atlasambiental[,3:11] %>% 
  cor() %>% 
  melt() %>% 
  rename(Correlação = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
            size = 3) +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# Teste da adequabilidade dos dados para PCA (Teste de esfericidade de Bartlett)
cortest.bartlett(atlasambiental[,3:11])

# Análise Fatorial por Componentes Principais (PCA)
fatorial <- principal(atlasambiental[3:11],
                      nfactors = length(atlasambiental[3:11]),
                      rotate = "none",
                      scores = TRUE)

# Identificação inicial de todos os autovalores
eigenvalues <- round(fatorial$values, 5)
print(eigenvalues)
sum(eigenvalues)

# Quantidade de autovalores maiores que 1 (critério de Kaiser)
k <- sum(eigenvalues > 1)
print(k)

# Definindo a Análise Fatorial por Componentes Principais (PCA) p/ 2 fatores
fatorial <- principal(atlasambiental[3:11],
                      nfactors = k,
                      rotate = "none",
                      scores = TRUE)

# Identificação da variância compartilhada em cada fator extraído
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Scree Plot com a proporção da variância compartilhada em cada fator
variancia_compartilhada %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.1))+
  labs(x = "Fatores",
       y = "Variância Compartilhada") +
  theme_bw()

# Extraindo as Cargas Fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))

# Visualizando as cargas fatoriais
cargas_fatoriais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Extraindo as Comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>%
  rename(comunalidades = 1)

# Visualizando as Comunalidades
comunalidades %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
cargas_fatoriais %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotagem das Cargas Fatoriais
cargas_fatoriais %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  theme_bw()

# Identificação dos Scores Fatoriais
scores_fatoriais <- as.data.frame(fatorial$weights)

# Visualizando os Scores Fatoriais
scores_fatoriais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Cálculo dos fatores extraídos
fatores <- as.data.frame(fatorial$scores)

atlasambiental <- bind_cols(atlasambiental,
                            "fator_1" = fatores$PC1, 
                            "fator_2" = fatores$PC2)

atlasambiental[,c(2, 12, 13)] %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Proposta de construção de um ranking

# Assumindo-se apenas o F1 como indicador, calcula-se a "pontuação"
# Trata-se do fator * variância compartilhada por aquele fator
atlasambiental <- atlasambiental %>% 
  mutate(pontuacao = fator_1 * variancia_compartilhada$PC1[2])

# Visualizando o ranking final em ordem decrescente
atlasambiental[,c(2, 14)] %>%
  arrange(desc(pontuacao)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotando o ranking de forma espacial

# Carregando o mapa do município de São Paulo
load(file = "mapa_sp.RData")

# Visualizando o mapa
tm_shape(mapa_sp) + 
  tm_borders()

# Acrescentando as informações da base de dados atlasambiental ao mapa
mapa_sp@data$COD_DIST <- as.numeric(mapa_sp@data$COD_DIST)

distritos_dados <- merge(mapa_sp,
                         atlasambiental,
                         by.x = "COD_DIST",
                         by.y = "cod_ibge")

# Plotando os rankings
# Modo interativo - para acionar o modo offline, basta argumentar "plot"
tmap_mode("view")  

tm_shape(distritos_dados) +
  tm_fill("pontuacao", midpoint = 0, palette = "RdBu", 
          style = "quantile", n = 10, legend.show = T) +
  tm_borders(alpha = 0.8) +
  tm_text("distritos")

# Fim!