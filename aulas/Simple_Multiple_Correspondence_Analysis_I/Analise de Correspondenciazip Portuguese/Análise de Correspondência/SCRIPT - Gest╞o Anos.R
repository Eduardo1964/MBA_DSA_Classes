
## Análise de Correspondência Simples
## Fonte: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 11

# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             "knitr", "kableExtra",
             "sjPlot",
             "FactoMineR",
             "amap", 
             "ade4")

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
load(file = "gestao_municipal.RData")

# Tabelas de frequência das variáveis
summary(gestao_municipal)

# Tabela de contingência com frequências absolutas observadas
sjt.xtab(var.row = gestao_municipal$ano,
         var.col = gestao_municipal$avaliação,
         show.exp = TRUE,
         encoding = "UTF-8")

# Tabela de contingência "base" para informações de resíduos
tabela_contingencia <- table(gestao_municipal$ano,
                             gestao_municipal$avaliação)

# Teste qui-quadrado para verificar a significância
qui2 <- chisq.test(x = tabela_contingencia)
qui2

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
                       high = "green",
                       midpoint = 1.96) +
  labs(x = 'Perfil', y = 'Aplicação', fill = "Res. Pad. Ajust.") +
  coord_flip() +
  theme_bw()

# O resultado pode ser obtido por meio da função 'CA' do pacote 'FactoMineR'
anacor <- CA(tabela_contingencia, graph = TRUE)

# Fim!