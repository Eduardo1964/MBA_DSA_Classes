
## Análise de Correspondência Simples

# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.

# Fonte: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais/indicadores-de-qualidade-da-educacao-superior

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap",
             "readxl",
             "gganimate",
             "gifski")

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
enade_2016 <- read_excel("conceito_enade_2016.xlsx")
enade_2021 <- read_excel("conceito_enade_2021.xlsx")

# Vamos excluir o ano e o id
enade_2016 <- enade_2016[,3:4]
enade_2021 <- enade_2021[,3:4]

# Tabelas de contingência para análise da associação entre categorias
sjt.xtab(var.row = enade_2016$Conceito_Enade,
         var.col = enade_2016$Categoria_Adm,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = enade_2021$Conceito_Enade,
         var.col = enade_2021$Categoria_Adm,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

# Gerando as Anacor
tab_cont_2016 <- table(enade_2016$Categoria_Adm, enade_2016$Conceito_Enade)
tab_cont_2021 <- table(enade_2021$Categoria_Adm, enade_2021$Conceito_Enade)
CA_2016 <- CA(tab_cont_2016, graph = F)
CA_2021 <- CA(tab_cont_2021, graph = F)

# Obtendo as coordenadas das categorias das variáveis
coord_2016 <- rbind(CA_2016$row$coord, CA_2016$col$coord)
coord_2021 <- rbind(CA_2021$row$coord, CA_2021$col$coord)

# Quantidade de categorias por variável
quant_categorias_2016 <- apply(enade_2016,
                               MARGIN =  2,
                               FUN = function(x) nlevels(as.factor(x)))

# Criando data frame com as coordenadas de 2016
df_CA_2016 <- data.frame(coord_2016[,1:2],
                         Variável = rep(names(quant_categorias_2016), quant_categorias_2016),
                         Ano = 2016) %>% rownames_to_column() %>% rename("Categorias" = 1)


# Quantidade de categorias por variável
quant_categorias_2021 <- apply(enade_2021,
                               MARGIN =  2,
                               FUN = function(x) nlevels(as.factor(x)))

# Criando data frame com as coordenadas de 2021
df_CA_2021 <- data.frame(coord_2021[,1:2],
                          Variável = rep(names(quant_categorias_2021), quant_categorias_2021),
                          Ano = 2021) %>% rownames_to_column() %>% rename("Categorias" = 1)
  
# Consolidando os data frames
df_CA_total <- rbind(df_CA_2016, df_CA_2021)

# Sobrepondo as coordenadas dos mapas perceptuais em um só plano
df_CA_total %>%
  ggplot() +
  geom_point(aes(x = Dim.1, y = Dim.2, color = Variável)) +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, 
                      label = Categorias,
                      color = Variável),
                  max.overlaps = 3000) +
  labs(x = "Dimensão 1",
       y = "Dimensão 2") +
  theme(legend.position = "none") -> mapas_perceptuais

# Definindo que a interação entre os mapas perceptuais se dará por ano
mapa_animado <- mapas_perceptuais + transition_time(Ano) +
  enter_fade() +
  labs(title = "Ano: {frame_time}") +
  exit_fade()

# Estabelecendo um fundo branco para os gráficos
theme_set(theme_light())

# Resultado final
animate(mapa_animado, renderer = gifski_renderer(), fps = 0.7, nframes = 2)

# Fim!