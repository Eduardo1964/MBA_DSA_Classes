
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
             "readxl") # importar arquivo Excel

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
NotasFatorial <- read_excel("notas_fatorial.xlsx")

# Visualização da base de dados
NotasFatorial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Estatísticas descritivas
summary(NotasFatorial)

# Scatter e ajuste linear entre as variáveis 'custos' e 'finanças'
NotasFatorial %>%
  ggplot() +
  geom_point(aes(x = finanças, y = custos),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = finanças, y = custos),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Finanças",
       y = "Custos") +
  theme_bw()

# Scatter e ajuste linear entre as variáveis 'custos' e 'marketing'
NotasFatorial %>%
  ggplot() +
  geom_point(aes(x = marketing, y = custos),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = marketing, y = custos),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Marketing",
       y = "Custos") +
  theme_bw()

# Scatter e ajuste linear entre as variáveis 'custos' e 'atuária'
NotasFatorial %>%
  ggplot() +
  geom_point(aes(x = atuária, y = custos),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = atuária, y = custos),
              color = "orange", 
              method = "lm", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "Atuária",
       y = "Custos") +
  theme_bw()

# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(NotasFatorial[,2:5]), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  NotasFatorial[,2:5] %>%
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
chart.Correlation(NotasFatorial[, 2:5], histogram = TRUE, pch = "+")

### Elaboração a Análise Fatorial Por Componentes Principais ###

# Teste de esfericidade de Bartlett
cortest.bartlett(NotasFatorial[, 2:5])

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(NotasFatorial[, 2:5],
                      nfactors = length(NotasFatorial[, 2:5]),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues

# Soma dos eigenvalues = 4 (quantidade de variáveis na análise)
# Também representa a quantidade máxima de possíveis fatores na análise
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

# Visualização das comunalidades (aqui são iguais a 1 para todas as variáveis)
# Foram extraídos 4 fatores neste primeiro momento
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

### Elaboração da Análise Fatorial por Componentes Principais ###
### Fatores extraídos a partir de autovalores maiores que 1 ###

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)

# Elaboração da análise fatorial por componentes principais
# Com quantidade 'k' de fatores com eigenvalues maiores que 1
fatorial2 <- principal(NotasFatorial[, 2:5],
                      nfactors = k,
                      rotate = "none",
                      scores = TRUE)
fatorial2

# Cálculo das comunalidades com apenas os 'k' ('k' = 2) primeiros fatores
comunalidades2 <- as.data.frame(unclass(fatorial2$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades com apenas os 'k' ('k' = 2) primeiros fatores
round(comunalidades2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Loading plot com as cargas dos 'k' ('k' = 2) primeiros fatores
cargas_fatoriais[, 1:2] %>% 
  data.frame() %>%
  rownames_to_column("variáveis") %>%
  ggplot(aes(x = PC1, y = PC2, label = variáveis)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "orange") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "orange") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()

# Adicionando os fatores extraídos no banco de dados original
NotasFatorial <- bind_cols(NotasFatorial,
                           "fator 1" = fatores$PC1, 
                           "fator 2" = fatores$PC2)

# Criação de um ranking Critério da soma ponderada e ordenamento)
NotasFatorial$ranking <- fatores$PC1 * variancia_compartilhada$PC1[2] +
                         fatores$PC2 * variancia_compartilhada$PC2[2]

# Visualizando o ranking final
NotasFatorial %>%
  arrange(desc(ranking)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 17)

# Fim!