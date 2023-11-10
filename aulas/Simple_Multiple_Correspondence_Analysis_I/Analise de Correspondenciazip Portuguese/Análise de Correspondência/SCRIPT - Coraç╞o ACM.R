
## Análise de Correspondência Múltipla

# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.

# Fonte: https://www.kaggle.com/code/jiagengchang/heart-disease-multiple-correspondence-analysis

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
dados_cor <- read_excel("dados_cor_acm.xlsx")

## Algumas variáveis são qualitativas e outras quantitativas

# Vamos categorizar as variáveis quanti (por critério estatístico)
dados_cor <- dados_cor %>% 
  mutate(Categ_Idade = case_when(Idade <= quantile(Idade, 0.25, na.rm = T) ~ "menores_idades",
                                 Idade > quantile(Idade, 0.25, na.rm = T) & Idade <= quantile(Idade, 0.75, na.rm = T) ~ "idades_médias",
                                 Idade > quantile(Idade, 0.75, na.rm = T) ~ "maiores_idades"))

dados_cor <- dados_cor %>% 
  mutate(Categ_PS_Desc = case_when(PS_Descanso <= quantile(PS_Descanso, 0.25, na.rm = T) ~ "PS_descanso_baixo",
                                   PS_Descanso > quantile(PS_Descanso, 0.25, na.rm = T) & PS_Descanso <= quantile(PS_Descanso, 0.75, na.rm = T) ~ "PS_descanso_médio",
                                   PS_Descanso > quantile(PS_Descanso, 0.75, na.rm = T) ~ "PS_descanso_alto"))

dados_cor <- dados_cor %>% 
  mutate(Categ_Colest = case_when(Colesterol <= quantile(Colesterol, 0.25, na.rm = T) ~ "menor_colesterol",
                                  Colesterol > quantile(Colesterol, 0.25, na.rm = T) & Colesterol <= quantile(Colesterol, 0.75, na.rm = T) ~ "colesterol_médio",
                                  Colesterol > quantile(Colesterol, 0.75, na.rm = T) ~ "maior_colesterol"))

dados_cor <- dados_cor %>% 
  mutate(Categ_BC_Max = case_when(BC_Max <= quantile(BC_Max, 0.25, na.rm = T) ~ "menor_BC_Max",
                                  BC_Max > quantile(BC_Max, 0.25, na.rm = T) & BC_Max <= quantile(BC_Max, 0.75, na.rm = T) ~ "BC_Max_médio",
                                  BC_Max > quantile(BC_Max, 0.75, na.rm = T) ~ "maior_BC_Max"))

# Vamos remover as variáveis que não utilizaremos (quantitativas)
dados_cor <- dados_cor %>% 
  select(-Idade, -PS_Descanso, -Colesterol, -BC_Max)

# A função para a criação da ACM pede que sejam utilizados "fatores"
dados_cor <- as.data.frame(unclass(dados_cor), stringsAsFactors=TRUE)

# Tabelas de contingência (todas apresentam associação com alguma variável?)
sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Sexo,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Tipo_Dor_Peito,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Açucar_Sangue,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$ECG_Descanso,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Angina_Exerc,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_Idade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_PS_Desc,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_Colest,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_BC_Max,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

# Vamos gerar a ACM
ACM <- dudi.acm(dados_cor, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(dados_cor,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
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

# Poderíamos fazer o mapa com as coordenadas obtidas por meio da matriz de Burt

# Consolidando as coordenadas-padrão obtidas por meio da matriz de Burt
df_ACM_B <- data.frame(ACM$co, Variável = rep(names(quant_categorias),
                                              quant_categorias))

# Plotando o mapa perceptual
df_ACM_B %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# É possível obter as coordenadas das observações
df_coord_obs <- ACM$li

# Plotando o mapa perceptual
df_coord_obs %>%
  ggplot(aes(x = Axis1, y = Axis2, color = dados_cor$Doença_Card)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%")),
       color = "Doença Cardíaca") +
  theme_bw()

# Fim!