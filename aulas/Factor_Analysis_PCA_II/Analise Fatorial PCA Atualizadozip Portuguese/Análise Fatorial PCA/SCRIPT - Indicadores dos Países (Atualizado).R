
## Análise Fatorial por Componentes Principais
## Fonte: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 10

# Curso: MBA DSA (USP ESALQ)
# Prof. Wilson Tarantin Jr.

# Instalação e carregamento dos pacotes utilizados

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc", # matriz de correlações com p-valor
             "readxl") # leitura de dados em Excel

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
base_indicador <- read_xlsx("indicador_país.xlsx")

# Visualização da base de dados
base_indicador %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

## Variáveis (1): referem-se ao 1º ano de análise
## Variáveis (2): referem-se ao 2º ano de análise

# Estatísticas descritivas
summary(base_indicador)

# Scatter e ajuste linear entre as variáveis 'escolaridade' e 'pib per capita'
base_indicador %>%
  ggplot() +
  geom_point(aes(x = escol1, y = pib_capita1),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = escol1, y = pib_capita1),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Escolaridade",
       y = "PIB per Capita") +
  theme_bw()

# Scatter e ajuste linear entre as variáveis 'escolaridade' e 'violência'
base_indicador %>%
  ggplot() +
  geom_point(aes(x = escol1, y = violência1),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = escol1, y = violência1),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Escolaridade",
       y = "Violência") +
  theme_bw()

### Elaboração da Análise Fatorial Por Componentes Principais (1º Ano) ###

# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(base_indicador[,2:5]), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  base_indicador[,2:5] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())

# Visualização das distribuições das variáveis, scatters, valores das correlações
chart.Correlation(base_indicador[, 2:5], histogram = TRUE, pch = "+")

# Teste de esfericidade de Bartlett
cortest.bartlett(base_indicador[, 2:5])

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(base_indicador[, 2:5],
                      nfactors = length(base_indicador[, 2:5]),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues
round(sum(eigenvalues), 2)

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos scores fatoriais
scores_fatoriais <- as.data.frame(fatorial$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos fatores propriamente ditos
fatores <- as.data.frame(fatorial$scores)

View(fatores)

# Coeficientes de correlação de Pearson para cada par de fatores (ortogonais)
rho <- rcorr(as.matrix(fatores), type="pearson")
round(rho$r, 4)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades (analisando os 4 fatores, são iguais a 1)
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

### Fatores extraídos a partir de autovalores maiores que 1 ###
### Análises para o 1º ano ###

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)

# Elaboração da análise fatorial por componentes principais
# Com quantidade 'k' de fatores com eigenvalues maiores que 1
fatorial1 <- principal(base_indicador[, 2:5],
                      nfactors = k,
                      rotate = "none",
                      scores = TRUE)
fatorial1

# Cálculo das cargas fatoriais
cargas_fatoriais1 <- as.data.frame(unclass(fatorial1$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Cálculo das comunalidades com o primeiro fator ('k' = 1)
comunalidades1 <- as.data.frame(unclass(fatorial1$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades
round(comunalidades1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Cálculo dos scores fatoriais
scores_fatoriais1 <- as.data.frame(fatorial1$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo do fator do 1º ano
fatores1 <- as.data.frame(fatorial1$scores)

# Adicionando o fator extraído ao banco de dados original
base_indicador <- bind_cols(base_indicador,
                           "fator_ano1" = fatores1$PC1)

### Elaboração da Análise Fatorial Por Componentes Principais (2º Ano) ###

# Teste de esfericidade de Bartlett
cortest.bartlett(base_indicador[, 6:9])

# Elaboração da análise fatorial por componentes principais
fatorial2 <- principal(base_indicador[, 6:9],
                       nfactors = length(base_indicador[, 6:9]),
                       rotate = "none",
                       scores = TRUE)
fatorial2

# Eigenvalues (autovalores)
eigenvalues2 <- round(fatorial2$values, 5)
print(eigenvalues2)

# Eigenvalues maiores do que 1
k2 <- sum(eigenvalues2 > 1)
print(k2)

# Parametrizando o algoritmo
fatorial2 <- principal(base_indicador[, 6:9],
                       nfactors = k2,
                       rotate = "none",
                       scores = TRUE)

# Extração dos scores fatoriais
scores_fatoriais2 <- as.data.frame(fatorial2$weights)

# Cálculo das cargas fatoriais
cargas_fatoriais2 <- as.data.frame(unclass(fatorial2$loadings))

# Cálculo do fator do 2º ano
fatores2 <- as.data.frame(fatorial2$scores)

# Adicionando o fator extraído ao banco de dados original
base_indicador <- bind_cols(base_indicador,
                            "fator_ano2" = fatores2$PC1)

# Cálculo das comunalidades com o primeiro fator ('k' = 1)
comunalidades2 <- as.data.frame(unclass(fatorial2$communality)) %>%
  rename(comunalidades = 1)

# Comparação dos scores fatoriais
compara_score <- round(cbind(scores_fatoriais1, scores_fatoriais2),3) %>% 
  rename(Fator_Ano1 = 1, Fator_Ano2 = 2)
rownames(compara_score) <- c("cpi",
                             "violência",
                             "pib_capita",
                             "escol")

# Comparação das cargas fatoriais
compara_cargas <- round(cbind(cargas_fatoriais1, cargas_fatoriais2),3) %>% 
  rename(Fator_Ano1 = 1, Fator_Ano2 = 2)
rownames(compara_cargas) <- c("cpi",
                              "violência",
                              "pib_capita",
                              "escol")

# Comparando os países entre os dois anos
base_indicador <- base_indicador %>% 
  arrange(desc(fator_ano1)) %>% 
  mutate(pos_ano1 = seq(fator_ano1)) %>% 
  arrange(desc(fator_ano2)) %>% 
  mutate(pos_ano2 = seq(fator_ano2))

# Fim!