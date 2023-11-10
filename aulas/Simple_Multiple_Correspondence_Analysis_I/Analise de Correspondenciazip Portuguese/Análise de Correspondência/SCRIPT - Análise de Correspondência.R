
## Análise de Correspondência Simples e Múltipla
## Fonte: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 11

# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "sjPlot", #elaboração de tabelas de contingência
             "FactoMineR", #função 'CA' para elaboração direta da Anacor
             "amap", #funções 'matlogic' e 'burt' para matrizes binária e de Burt
             "ade4") #função 'dudi.acm' para elaboração da ACM

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
load(file = "perfil_investidor_aplicacao.RData")

# Visualização da base de dados
perfil_investidor_aplicacao %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Tabelas de frequência das variáveis
summary(perfil_investidor_aplicacao)

#----------------- Análise de Correspondência Simples (ANACOR) -----------------

## 1ª Parte: Análise da associação por meio de tabelas

# Tabela de contingência com frequências absolutas observadas
tabela_contingencia <- table(perfil_investidor_aplicacao$perfil,
                             perfil_investidor_aplicacao$aplicacao)
tabela_contingencia

# Definição da quantidade de observações na tabela de contingência
n <- sum(tabela_contingencia)
n

# Estatística qui-quadrado e teste
qui2 <- chisq.test(x = tabela_contingencia)
qui2

# Tabela de contingência com frequências absolutas observadas
qui2$observed

# Tabela de contingência com frequências absolutas esperadas
qui2$expected

# Tabela de contingência com frequências absolutas observadas e esperadas
sjt.xtab(var.row = perfil_investidor_aplicacao$perfil,
         var.col = perfil_investidor_aplicacao$aplicacao,
         show.exp = TRUE)

# Resíduos – diferenças entre frequências absolutas observadas e esperadas
qui2$observed - qui2$expected

# Valores de qui-quadrado por célula
((qui2$observed - qui2$expected)^2)/qui2$expected

# Resíduos padronizados
qui2$residuals

# Resíduos padronizados ajustados
qui2$stdres

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(perfil = 1,
         aplicacao = 2) %>% 
  ggplot(aes(x = fct_rev(perfil), y = aplicacao,
             fill = Freq, label = round(Freq, 3))) +
  geom_tile() +
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", 
                       mid = "white", 
                       high = "purple",
                       midpoint = 1.96) +
  labs(x = 'Perfil', y = 'Aplicação', fill = "Res. Pad. Ajustados") +
  coord_flip() +
  theme_bw()

## 2ª Parte: Análise da associação por meio do mapa perceptual

# Definição da matriz A
# Resíduos padronizados (qui2$residuals) divididos pela raiz quadrada do tamanho da amostra (n)
matrizA <- qui2$residuals/sqrt(n)
matrizA

# Definição da matriz W
# Multiplicação da matriz A transposta pela matriz A
matrizW <- t(matrizA) %*% matrizA
matrizW

# Definição da quantidade de dimensões
qtde_dimensoes <- min(nrow(matrizW) - 1, ncol(matrizW) - 1)
qtde_dimensoes

# Definição dos valores singulares
VS_AV <- svd(matrizA, nu = qtde_dimensoes, nv = qtde_dimensoes)

# Valores singulares de cada dimensão
valores_singulares <- VS_AV$d[1:qtde_dimensoes]
valores_singulares

# Autovalores (eigenvalues) de cada dimensão
eigenvalues <- (valores_singulares)^2
eigenvalues

# Cálculo da inércia principal total (a partir do qui-quadrado)
inercia_total <- as.numeric(qui2$statistic/sum(tabela_contingencia))
inercia_total

# Cálculo da variância explicada em cada dimensão
variancia_explicada <- eigenvalues / inercia_total
variancia_explicada

# Cálculo das massas das colunas (column profiles)
soma_colunas <- apply(tabela_contingencia, MARGIN = 1, FUN = sum)
soma_colunas

# Massas das colunas (column profiles)
massa_colunas <- soma_colunas / n
massa_colunas

# Cálculo das massas das linhas (row profiles)
soma_linhas <- apply(tabela_contingencia, MARGIN = 2, FUN = sum)
soma_linhas

# Massas das linhas (row profiles)
massa_linhas <- soma_linhas / n
massa_linhas

# Autovetores v das dimensões
autovetor_v <-VS_AV$v
autovetor_v

# Autovetores u das dimensões
autovetor_u <-VS_AV$u
autovetor_u

# Resumindo as informações até aqui
data.frame(Dimensão = paste("Dimensão", 1:qtde_dimensoes),
           `Valor Singular` = valores_singulares,
           `Inércia Principal Parcial eigenvalues` = eigenvalues) %>%
  mutate(`Percentual da Inércia Principal Total` = (`Inércia.Principal.Parcial.eigenvalues`/inercia_total) * 100,
         `Percentual da Inércia Principal Total Acumulada` = cumsum(`Percentual da Inércia Principal Total`),
         Qui2 = qui2$statistic[[1]] * `Percentual da Inércia Principal Total` / n,
         `Valor Singular` = `Valor.Singular`,
         `Inércia Principal Parcial eigenvalues` = Inércia.Principal.Parcial.eigenvalues) %>%
  select(Dimensão, `Valor Singular`, `Inércia Principal Parcial eigenvalues`,
         Qui2, `Percentual da Inércia Principal Total`,
         `Percentual da Inércia Principal Total Acumulada`) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 17)

# Calculando as coordenadas para plotar as categorias no mapa perceptual

# Variável em linha na tabela de contingência ('perfil')
# Coordenadas das abcissas
coord_abcissas_perfil <- sqrt(valores_singulares[1]) * (massa_colunas^-0.5) * autovetor_u[,1]
coord_abcissas_perfil

# Coordenadas das ordenadas
coord_ordenadas_perfil <- sqrt(valores_singulares[2]) * (massa_colunas^-0.5) * autovetor_u[,2]
coord_ordenadas_perfil

# Variável em coluna na tabela de contingência ('aplicacao')
# Coordenadas das abcissas
coord_abcissas_aplicacao <- sqrt(valores_singulares[1]) * (massa_linhas^-0.5) * autovetor_v[,1]
coord_abcissas_aplicacao

# Coordenadas das ordenadas
coord_ordenadas_aplicacao <- sqrt(valores_singulares[2]) * (massa_linhas^-0.5) * autovetor_v[,2]
coord_ordenadas_aplicacao

# Mapa perceptual
cbind.data.frame(coord_abcissas_perfil, coord_ordenadas_perfil,
                 coord_abcissas_aplicacao, coord_ordenadas_aplicacao) %>%
  rename(dim_1_perfil = 1,
         dim_2_perfil = 2,
         dim_1_aplicacao = 3,
         dim_2_aplicacao = 4) %>%
  rownames_to_column() %>%
  setNames(make.names(names(.), unique = TRUE)) %>%
  mutate(aplicacao = rownames(data.frame(coord_abcissas_aplicacao,
                                         coord_ordenadas_aplicacao))) %>%
  rename(perfil = 1,
         dim_1_perfil = 2,
         dim_2_perfil = 3,
         dim_1_aplicacao = 4,
         dim_2_aplicacao = 5) %>%
  ggplot() +
  geom_point(aes(x = dim_1_perfil, y = dim_2_perfil),
             color = "deeppink1",
             fill = "deeppink1",
             shape = 24,
             size = 4) +
  geom_text_repel(aes(x = dim_1_perfil, y = dim_2_perfil, label = perfil)) +
  geom_point(aes(x = dim_1_aplicacao, y = dim_2_aplicacao),
             color = "turquoise3",
             fill = "turquoise3",
             shape = 21,
             size = 4) +
  geom_text_repel(aes(x = dim_1_aplicacao, y = dim_2_aplicacao, label = aplicacao)) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(variancia_explicada[1] * 100, 2),"%")),
       y = paste("Dimensão 2:", paste0(round(variancia_explicada[2] * 100, 2),"%"))) +
  theme_bw()

# O resultado pode ser obtido por meio da função 'CA' do pacote 'FactoMineR'
anacor <- CA(tabela_contingencia, graph = TRUE)

# Note que a função 'CA' gera um mapa perceptual construído com coordenadas
# definidas de maneira diferente em relação às calculadas antes.

# Entretanto, as proporções das proximidades entre as categorias das variáveis
# permanecem as mesmas, assim como os percentuais da inércia principal total
# por dimensão!

#------------------ Análise de Correspondência Múltipla (ACM) ------------------

# Carregamento da base de dados
load(file = "perfil_investidor_aplicacao_estadocivil.RData")

# Visualização da base de dados
perfil_investidor_aplicacao_estadocivil %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Tabelas de frequência das variáveis qualitativas
summary(perfil_investidor_aplicacao_estadocivil)

# Tabelas de contingência
# Perfil x Aplicação
sjt.xtab(var.row = perfil_investidor_aplicacao_estadocivil$perfil,
         var.col = perfil_investidor_aplicacao_estadocivil$aplicacao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Perfil x Estado Civil
sjt.xtab(var.row = perfil_investidor_aplicacao_estadocivil$perfil,
         var.col = perfil_investidor_aplicacao_estadocivil$estado_civil,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Aplicação x Estado Civil
sjt.xtab(var.row = perfil_investidor_aplicacao_estadocivil$aplicacao,
         var.col = perfil_investidor_aplicacao_estadocivil$estado_civil,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Matriz Binária e Matriz de Burt

# Matriz binária
matriz_binaria <- matlogic(perfil_investidor_aplicacao_estadocivil[,2:4])
matriz_binaria

# Para a matriz de Burt
matriz_burt <- burt(perfil_investidor_aplicacao_estadocivil[,2:4])
matriz_burt

verifica_burt <- t(matriz_binaria) %*% matriz_binaria

# Elaboração da análise de correspondência múltipla (ACM)
ACM <- dudi.acm(perfil_investidor_aplicacao_estadocivil[,2:4], scannf = FALSE)

# Visualização das coordenadas principais das categorias das variáveis
# Método da matriz de Burt B (componente 'co' do objeto 'ACM')
round(ACM$co, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Visualização das coordenadas-padrão das categorias das variáveis
# Método da matriz binária (componente 'c1' do objeto 'ACM')
round(ACM$c1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Massas das linhas e colunas (componente 'cw' do objeto 'ACM')
ACM$cw

# Inércias principais (componente 'eig' do objeto 'ACM')
ACM$eig
## Portanto, há 5 dimensões. J-Q = 8-3 = 5

# Percentual de variância explicada por dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Visualização do percentual de variância explicada por dimensão
data.frame(Dimensão = paste("Dimensão", 1:length(perc_variancia)),
           Variância = perc_variancia) %>%
  ggplot(aes(x = Dimensão,
             y = Variância,
             label = paste0(round(Variância, 2),"%"))) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_text(vjust = 2.5, size = 5) +
  theme_bw()

# Mapa perceptual na ACM

# Definição da quantidade de categorias de cada variável qualitativa
quant_categorias <- apply(perfil_investidor_aplicacao_estadocivil[,2:4],
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária ('c1')
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Visualizando as coordenadas
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("perfil.","", Categoria),
         Categoria = gsub("aplicacao.","", Categoria),
         Categoria = gsub("estado_civil.","", Categoria)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("perfil.","", Categoria),
         Categoria = gsub("aplicacao.","", Categoria),
         Categoria = gsub("estado_civil.","", Categoria)) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_color_manual("Variável",
                     values = c("turquoise3", "springgreen4", "deeppink1")) +
  theme_bw()

# Coletando as coordenadas das observações
ACM_observacoes_df <- data.frame(ACM$li)

# Vamos acrescentar as informações das observacões ao mapa perceptual da ACM
ACM_observacoes_df %>% 
  ggplot(aes(x = Axis1, y = Axis2, label = perfil_investidor_aplicacao$estudante)) +
  geom_point(shape = 17, color = "red", size = 2) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey48") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey48") +
  geom_text_repel(max.overlaps = 100, size = 3) +
  geom_density2d(color = "gray") +
  geom_label_repel(data = df_ACM, 
                   aes(x = CS1, y = CS2, 
                       label = rownames(df_ACM), 
                       fill = Variável), 
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

# Fim!