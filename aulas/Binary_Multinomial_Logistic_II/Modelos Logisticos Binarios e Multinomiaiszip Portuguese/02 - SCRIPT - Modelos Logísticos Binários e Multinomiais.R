##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


##############################################################################
#                   REGRESSÃO LOGÍSTICA BINÁRIA - PARTE CONCEITUAL           #
##############################################################################
#Estabelecendo uma função para a probabilidade de ocorrência de um evento
prob <- function(z){
  prob = 1 / (1 + exp(-z))
}

#Plotando a curva sigmóide teórica de ocorrência de um evento para um range
#do logito z entre -5 e +5
data.frame(z = -5:5) %>%
  ggplot() +
  stat_function(aes(x = z, color = "Prob. Evento"),
                fun = prob,
                size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Logito z",
       y = "Probabilidade") +
  theme_bw()


##############################################################################
#                       REGRESSÃO LOGÍSTICA BINÁRIA                          #                  
#                EXEMPLO 01 - CARREGAMENTO DA BASE DE DADOS                  #
##############################################################################
load(file = "Atrasado.RData")

##############################################################################
#            EXEMPLO 01 - OBSERVAÇÃO DA BASE DE DADOS Atrasado               #
##############################################################################
#Visualizando a base de dados
Atrasado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Estatísticas descritivas univariadas da base de dados
summary(Atrasado)

#Tabela de frequências absolutas da variável 'atrasado'
table(Atrasado$atrasado) 

##############################################################################
#           EXEMPLO 01 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO            #
##############################################################################
modelo_atrasos <- glm(formula = atrasado ~ dist + sem, 
                      data = Atrasado, 
                      family = "binomial")

#Parâmetros do modelo_atrasos
summary(modelo_atrasos)

#Visualização do modelo no ambiente Viewer
#função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_atrasos, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Note que o summary() do modelo não traz a estatística geral do modelo,
#nem tampouco do valor de LL e dos intervalos de confiança.

# Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_atrasos, level = 0.95)

#Extração do valor de Log-Likelihood (LL)
logLik(modelo_atrasos)

# Outras maneiras de apresentar os outputs do modelo
#Funções 'summ' e 'export_summs' do pacote 'jtools' e função 'stargazer' do
#pacote 'stargazer'
summ(modelo_atrasos, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_atrasos, scale = F, digits = 6)
stargazer(modelo_atrasos, nobs = T, type = "text") # mostra o valor de Log-Likelihood

#Fazendo predições para o modelo_atrasos. Exemplo: qual a probabilidade média
#de se chegar atrasado quando o trajeto tem 7 km e passa-se por 10 semáforos no percurso?
predict(object = modelo_atrasos, 
        data.frame(dist = 7, sem = 10), 
        type = "response")

##############################################################################
#               EXEMPLO 01 - CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO            #
##############################################################################
# Adicionando os valores previstos de probabilidade na base de dados
Atrasado$phat <- modelo_atrasos$fitted.values

#Visualizando a base de dados com a variável 'phat'
Atrasado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Ajuste linear entre a variável dependente e a variável 'sem' (apenas para fins
#didáticos)
ggplotly(
  Atrasado %>% 
    ggplot() +
    geom_point(aes(x = sem, y = atrasado), color = "orange", size = 2) +
    geom_smooth(aes(x = sem, y = phat), 
                method = "lm", formula = y ~ x,
                se = FALSE,
                color = "darkorchid", size = 2) +
    labs(x = "Quantidade de Semáforos",
         y = "Atrasado") +
    theme_bw()
)

# Ajuste logístico determinístico entre a variável dependente e a variável 'sem'
#Sigmóide
ggplotly(
  Atrasado %>% 
    ggplot() +
    geom_point(aes(x = sem, y = atrasado), color = "orange", size = 2) +
    geom_smooth(aes(x = sem, y = phat), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = FALSE,
                color = "darkorchid", size = 2) +
    labs(x = "Quantidade de Semáforos",
         y = "Atrasado") +
    theme_bw()
)

# Ajuste logístico probabilístico entre a variável dependente e a variável 'sem'
#Sigmóide
ggplotly(
  Atrasado %>% 
    ggplot() +
    geom_point(aes(x = sem, y = phat), color = "orange", size = 2) +
    geom_smooth(aes(x = sem, y = phat), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = FALSE,
                color = "darkorchid", size = 2) +
    labs(x = "Quantidade de Semáforos",
         y = "Atrasado") +
    theme_bw()
)

#Matriz de confusão para cutoff = 0.5 (função confusionMatrix do pacote caret)
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.5,
                      Atrasado$atrasado == 1)[2:1, 2:1])

#Visualizando os principais indicadores desta matriz de confusão
data.frame(Sensitividade = confusionMatrix(table(predict(modelo_atrasos,
                                                         type = "response") >= 0.5,
                                          Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo_atrasos,
                                                          type = "response") >= 0.5,
                                          Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(modelo_atrasos,
                                                    type = "response") >= 0.5,
                                          Atrasado$atrasado == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)

#Matriz de confusão para cutoff = 0.3
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.3,
                      Atrasado$atrasado == 1)[2:1, 2:1])

#Matriz de confusão para cutoff = 0.7
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.7,
                      Atrasado$atrasado == 1)[2:1, 2:1])

##############################################################################
#  EXEMPLO 01 - IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE  #
##############################################################################
#Tentaremos estabelecer um critério que iguale a probabilidade de acerto
#daqueles que chegarão atrasados (sensitividade) e a probabilidade de acerto
#daqueles que não chegarão atrasados (especificidade).

#ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
#FORMA o procedimento garante a maximização da acurácia do modelo!

#função prediction do pacote ROCR
predicoes <- prediction(predictions = modelo_atrasos$fitted.values, 
                        labels = as.factor(Atrasado$atrasado))
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
#A função peformance(), do pacote ROCR, extrai do objeto 'predicoes' os 
#dados de sensitividade e de especificidade para a plotagem.

#Desejamos os dados da sensitividade e de especificidade. Então, devemos
#digitar os seguintes códigos:

sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

#Extraindo os cutoffs:
cutoffs <- dados_curva_roc@x.values[[1]] 

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Visualizando o novo dataframe dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

##############################################################################
#                       EXEMPLO 01 - CONSTRUÇÃO DA CURVA ROC                 #
##############################################################################
#função roc do pacote pROC
ROC <- roc(response = Atrasado$atrasado, 
           predictor = modelo_atrasos$fitted.values)

#Plotagem da curva ROC propriamente dita
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


##############################################################################
#             REGRESSÃO LOGÍSTICA BINÁRIA E PROCEDIMENTO STEPWISE            #        
#                 EXEMPLO 02 - CARREGAMENTO DA BASE DE DADOS                 #
##############################################################################
load("challenger.RData")

##############################################################################
#            EXEMPLO 02 - OBSERVAÇÃO DA BASE DE DADOS CHALLENGER             #
##############################################################################
#Visualizando a base de dados challenger
challenger %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#desgaste: quantidade de vezes em que ocorreu stress térmico
#temperatura: temperatura de lançamento (graus ºF)
#pressão: pressão de verificação de vazamento (psi-libra-força por polegada ao quadrado)
#t: teste para o lançamento (id)

#Estatísticas univariadas descritivas da base de dados
summary(challenger)

##############################################################################
#             EXEMPLO 02 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO          #
##############################################################################

#Não há uma variável binária para servir como uma variável dependente, certo?
#Então vamos criá-la considerando a ocorrência de desgastes de peças como a
#ocorrência de um evento que chamaremos de 'falha':
challenger %>%
  mutate(falha = ifelse(desgaste > 0,
                        yes = "sim",
                        no = "não"),
         falha = factor(falha)) -> challenger

#Vamos observar as alterações na base de dados original:
challenger %>%
  select(desgaste, falha, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Estimando o modelo logístico binário
modelo_challenger <- glm(formula = falha ~ . -desgaste -t,
                         data = challenger,
                         family = "binomial")

#Parâmetros do modelo_default
summary(modelo_challenger)
#Note que não há a explicitação da estatística geral do modelo,
#nem tampouco do valor de LL e dos intervalos de confiança.

#Uma solução rápida para o caso pode ser a utilização da função summ do pacote jtools
summ(model = modelo_challenger, confint = T, digits = 4, ci.width = 0.95)
export_summs(modelo_challenger, scale = F, digits = 4)

#Procedimento Stepwise
step_challenger <- step(object = modelo_challenger,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_challenger
summ(model = step_challenger, confint = T, digits = 4, ci.width = 0.95)

#Fazendo predições para o modelo step_challenger:
#Exemplo 1: qual a probabilidade média de falha a 70ºF (~21ºC)?
predict(object = step_challenger,
        data.frame(temperatura = 70),
        type = "response")

#Exemplo 2: qual a probabilidade média de falha a 77ºF (25ºC)?
predict(object = step_challenger,
        data.frame(temperatura = 77),
        type = "response")

#Exemplo 3: qual a probabilidade média de falha a 34ºF (~1ºC) - manhã do lançamento?
predict(object = step_challenger,
        data.frame(temperatura = 34),
        type = "response")

#Construção da sigmoide - probabilidade de evento em função da variável 'temperatura'
ggplotly(
  challenger %>% 
  mutate(phat = predict(object = step_challenger,
                        newdata = challenger,
                        type = "response"),
         falha = as.numeric(falha) - 1) %>% 
  ggplot() +
  geom_point(aes(x = temperatura, y = falha), color = "#95D840FF", size = 2) +
  geom_smooth(aes(x = temperatura, y = phat), 
              method = "glm", formula = y ~ x, 
              method.args = list(family = "binomial"), 
              se = F,
              color = "#440154FF", size = 2) +
  labs(x = "Temperatura",
       y = "Falha") +
  theme_bw()
)

#Nossa homenagem aos astronautas
image_scale(image_read("https://img.ibxk.com.br///2016/01/29/29182307148581.jpg?w=1200&h=675&mode=crop&scale=both"),
            "x320")


##############################################################################
#   REGRESSÃO LOGÍSTICA BINÁRIA COM VARIÁVEIS EXPLICATIVAS QUANTI E QUALIS   #
#                 EXEMPLO 03 - CARREGAMENTO DA BASE DE DADOS                 #
##############################################################################
load("dados_fidelidade.RData")

##############################################################################
#        EXEMPLO 03 - OBSERVAÇÃO DA BASE DE DADOS DADOS_FIDELIDADE           #
##############################################################################
#Visualizando a base de dados dados_fidelidade
dados_fidelidade %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 13)

#Estatísticas Univariadas da Base de Dados
summary(dados_fidelidade)

#Tabela de frequências absolutas das variáveis qualitativas referentes aos
#atributos da loja na percepção dos consumidores
table(dados_fidelidade$atendimento)
table(dados_fidelidade$sortimento)
table(dados_fidelidade$acessibilidade)
table(dados_fidelidade$preço)

glimpse(dados_fidelidade)
#Note que as variáveis qualitativas já estão como fator (fct)

##############################################################################
#             EXEMPLO 03 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO BINÁRIO          #
##############################################################################
modelo_fidelidade <- glm(formula = fidelidade ~ . - id, 
                         data = dados_fidelidade, 
                         family = "binomial")

#Parâmetros do modelo_fidelidade
summary(modelo_fidelidade)

#Outro modo de apresentar os outputs do modelo_fidelidade
summ(modelo_fidelidade, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_fidelidade, scale = F, digits = 6)

#Procedimento Stepwise
step_fidelidade <- step(object = modelo_fidelidade,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_fidelidade
summary(step_fidelidade)
#Note que sem a dummização, o R consegue calcular corretamente os parâmetros,
#mas o procedimento Stepwise, quando aplicado, não surte efeitos!

##############################################################################
#                    EXEMPLO 03 -  PROCEDIMENTO N-1 DUMMIES                  #
##############################################################################
#Dummizando as variáveis atendimento, sortimento, acessibilidade e preço. O 
#código abaixo, automaticamente, fará: a) a dummização das variáveis originais;
#b)removerá as variáveis dummizadas originais; c) estabelecerá como categorias 
#de referência as categorias de label 1 de cada variável original.
fidelidade_dummies <- dummy_columns(.data = dados_fidelidade,
                                    select_columns = c("atendimento", 
                                                       "sortimento",
                                                       "acessibilidade", 
                                                       "preço"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = T)

##############################################################################
#                      EXEMPLO 03 -  REESTIMANDO O MODELO                    #
##############################################################################
#Visualizando a base de dados fidelidade_dummies
fidelidade_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

modelo_fidelidade_dummies <- glm(formula = fidelidade ~ . -id, 
                                 data = fidelidade_dummies, 
                                 family = "binomial")

#Parâmetros do modelo_fidelidade_dummies
summary(modelo_fidelidade_dummies)

#Valor do LL do modelo_fidelidade_dummies
logLik(modelo_fidelidade_dummies)

#Procedimento Stepwise
step_fidelidade_dummies <- step(object = modelo_fidelidade_dummies,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_fidelidade_dummies
summary(step_fidelidade_dummies)

#Outro modo de apresentar os outputs do modelo step_fidelidade_dummies
summ(step_fidelidade_dummies, confint = T, digits = 3, ci.width = .95)
export_summs(step_fidelidade_dummies, scale = F, digits = 6)

#Valor do LL do modelo step_fidelidade_dummies
logLik(step_fidelidade_dummies)

#Comparando os modelos step_fidelidade_dummies e modelo_fidelidade_dummies
#função lrtest do pacote lmtest
lrtest(modelo_fidelidade_dummies, step_fidelidade_dummies)

export_summs(modelo_fidelidade_dummies, step_fidelidade_dummies,
             model.names = c("Modelo Dummies","Modelo Dummies Stepwise"),
             scale = F, digits = 4)

##############################################################################
#              EXEMPLO 03 - CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO             #
##############################################################################
confusionMatrix(
  table(predict(step_fidelidade_dummies, type = "response") >= 0.5, 
                      dados_fidelidade$fidelidade == "sim")[2:1, 2:1]
  )

##############################################################################
#  EXEMPLO 03 - IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE  #
##############################################################################
#Analogamente ao realizado para o Exemplo 01, vamos estabelecer um critério
#que iguale a probabilidade de acerto daqueles que apresentarão fidelização ao
#estabelecimento varejista (sensitividade) e a probabilidade de acerto daqueles
#que não apresentarão fidelização (especificidade).

#ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
#FORMA o procedimento garante a maximização da acurácia do modelo!

#função prediction do pacote ROCR
predicoes <- prediction(predictions = step_fidelidade_dummies$fitted.values, 
                        labels = dados_fidelidade$fidelidade) 
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
#A função peformance(), do pacote ROCR, extraiu do objeto 'predicoes' os 
#dados de sensitividade, de sensibilidade e de especificidade para a plotagem.

#Porém, desejamos os dados da sensitividade, então devemos fazer o seguinte 
#ajuste:
sensitividade <- dados_curva_roc@y.values[[1]] 
#extraindo dados da sensitividade do modelo

especificidade <- performance(predicoes, measure = "spec") 
#extraindo os dados da especificidade, mas também há que se fazer um ajuste para a 
#plotagem:
especificidade <- especificidade@y.values[[1]]

cutoffs <- dados_curva_roc@x.values[[1]] 
#extraindo os cutoffs do objeto 'sensitividade'.

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Visualizando o novo dataframe dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

##############################################################################
#                     EXEMPLO 03 - CONSTRUÇÃO DA CURVA ROC                   #
##############################################################################
ROC <- roc(response = dados_fidelidade$fidelidade, 
           predictor = step_fidelidade_dummies$fitted.values)

#Plotagem da curva ROC propriamente dita
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


##############################################################################
#                       REGRESSÃO LOGÍSTICA MULTINOMIAL                      #
#                 EXEMPLO 04 - CARREGAMENTO DA BASE DE DADOS                 #
##############################################################################
load(file = "AtrasadoMultinomial.RData")

##############################################################################
#        EXEMPLO 04 - OBSERVAÇÃO DA BASE DE DADOS AtrasadoMultinomial        #
##############################################################################
#Visualizando a base de dados AtrasadoMultinomial
AtrasadoMultinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas univariadas da base de dados
summary(AtrasadoMultinomial)

##############################################################################
#           EXEMPLO 04 - ESTIMAÇÃO DE UM MODELO LOGÍSTICO MULTINOMIAL        #
##############################################################################
#Apontando a categoria de referência
AtrasadoMultinomial$atrasado <- relevel(AtrasadoMultinomial$atrasado, 
                                  ref = "nao chegou atrasado")

#Estimação do modelo - função multinom do pacote nnet
modelo_atrasado <- multinom(formula = atrasado ~ dist + sem, 
                            data = AtrasadoMultinomial)

#Parâmetros do modelo_atrasado
summary(modelo_atrasado)

#Outra maneira de apresentar os outputs do modelo
#função stargazer do pacote stargazer -> mais adequado que a função export_summs
stargazer(modelo_atrasado, nobs=T, type="text")

#LL do modelo_atrasado
logLik(modelo_atrasado)

#A função summ do pacote jtools não funciona para objetos de classe 'multinom'. Logo,
#vamos definir uma função Qui2 para se extrair a estatística geral do modelo:
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

#Estatística geral do modelo_atrasado
Qui2(modelo_atrasado)

#Na Regressão Logística Multinomial, o R quebra a lógica de relatórios que, 
#normalmente, oferece para os GLM. Também é preciso notar que a linguagem 
#básica  não consegue rodar esse tipo de regressão, sendo necessário o pacote
#nnet. Além do mais, não são fornecidas as estatísticas z de Wald, nem os
#p-values das variáveis da modelagem.

#Explicando a lógica do R para a Logística Multinomial:

#1 - Foram estabelecidas *labels* para as categorias da variável dependente: 
#'nao chegou atrasado', 'chegou atrasado à primeira aula' e 'chegou atrasado à
#segunda aula';

#2 - Foi comandado que a categoria de referência seria a categoria 'nao chegou
#atrasado', e isso explica o porquê dela não aparecer no relatório gerado;

#3 - O relatório é dividido em duas partes: 'Coefficients' e 'Std. Errors'. 
#Cada linha da seção 'Coefficients' informa um logito para cada categoria da
#variável dependente, com exceção da categoria de referência. Já a seção 
#'Std. Errors' informa o erro-padrão de cada parâmetro em cada logito.

#Para calcular as estatísticas z de Wald, há que se dividir os valores da 
#seção 'Coefficients' pelos valores da seção 'Std. Errors.' Assim, temos que:  

zWald_modelo_atrasado <- (summary(modelo_atrasado)$coefficients / 
                            summary(modelo_atrasado)$standard.errors)

zWald_modelo_atrasado

#Porém, ainda faltam os respectivos p-values. Assim, os valores das probabilidades 
#associadas às abscissas de uma distribuição normal-padrão é dada pela função
#pnorm(), considerando os valores em módulo - abs(). Após isso, multiplicamos 
#por dois os valores obtidos para considerar os dois lados da distribuição
#normal padronizada (distribuição bicaudal). Desta forma, temos que:
round((pnorm(abs(zWald_modelo_atrasado), lower.tail = F) * 2), 4)

#Fazendo predições para o modelo_atrasado. Exemplo: qual a probabilidade média
#de atraso para cada categoria da variável dependente, se o indivíduo tiver 
#que percorrer 22km e passar por 12 semáforos?
predict(modelo_atrasado, 
        data.frame(dist = 22, sem = 12), 
        type = "probs")

predict(modelo_atrasado, 
        data.frame(dist = 22, sem = 12), 
        type = "class")

##############################################################################
#                   EXEMPLO 04 - A EFETIVIDADE GERAL DO MODELO               #
##############################################################################
#Adicionando as prováveis ocorrências de evento apontadas pela modelagem à 
#base de dados
AtrasadoMultinomial$predicao <- predict(modelo_atrasado, 
                                     newdata = AtrasadoMultinomial, 
                                     type = "class")

#Visualizando a nova base de dados AtrasadoMultinomial com a variável 'predicao'
AtrasadoMultinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

attach(AtrasadoMultinomial)

#Criando uma tabela para comparar as ocorrências reais com as predições
EGM <- as.data.frame.matrix(table(predicao, atrasado))

#Visualizando a tabela EGM
EGM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Eficiência global do modelo
acuracia <- (round((sum(diag(table(atrasado, predicao))) / 
                      sum(table(atrasado, predicao))), 2))

acuracia

##############################################################################
#                      EXEMPLO 04 - PLOTAGENS DAS PROBABILIDADES             #
##############################################################################

#Adicionando à base de dados as probabilidades em razão de cada categoria:
levels(AtrasadoMultinomial$atrasado)

AtrasadoMultinomial[c("nao chegou atrasado",
                      "chegou atrasado à primeira aula",
                      "chegou atrasado à segunda aula")] <- modelo_atrasado$fitted.values

#Plotagem das smooth probability lines para a variável 'dist'
ggplotly(
  AtrasadoMultinomial %>% 
    dplyr::select(-predicao, - estudante) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","dist","sem"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c("nao chegou atrasado",
                                          "chegou atrasado à primeira aula",
                                          "chegou atrasado à segunda aula"))) %>% 
    ggplot() +
    geom_smooth(aes(x = dist, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Distância Percorrida",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)

#Plotagem das smooth probability lines para a variável 'sem'
ggplotly(
  AtrasadoMultinomial %>% 
    dplyr::select(-predicao, - estudante) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","dist","sem"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c("nao chegou atrasado",
                                          "chegou atrasado à primeira aula",
                                          "chegou atrasado à segunda aula"))) %>% 
    ggplot() +
    geom_smooth(aes(x = sem, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Semáforos no Percurso",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)

#Plotagem tridimensional para cada probabilidade de ocorrência de cada
#categoria da variável dependente

AtrasadoMultinomial$p0 <- AtrasadoMultinomial$`nao chegou atrasado`
AtrasadoMultinomial$p1 <- AtrasadoMultinomial$`chegou atrasado à primeira aula`
AtrasadoMultinomial$p2 <- AtrasadoMultinomial$`chegou atrasado à segunda aula`


#p0 - Probabilidades de não chegar atrasado (função scatter3d do pacote car):
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p0,
          AtrasadoMultinomial$sem,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`nao chegou atrasado`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`nao chegou atrasado`,
        colors = colorRamp(c("red","yellow","chartreuse3","lightblue","blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria nao chegou Atrasado")


#p1 - Probabilidades de chegar atrasado à primeira aula:
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p1,
          AtrasadoMultinomial$sem,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
        colors = colorRamp(c("red", "yellow", "chartreuse3", "lightblue", "blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria Chegou Atrasado à Primeira Aula")


#p2 - Probabilidades de chegar atrasado à segunda aula:
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p2,
          AtrasadoMultinomial$sem,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
        colors = colorRamp(c("red", "yellow", "chartreuse3", "lightblue", "blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria Chegou Atrasado à Segunda Aula")


#Visualização das sigmóides tridimensionais em um único gráfico:
naoatrasado <- plot_ly(x = AtrasadoMultinomial$dist, 
                       y = AtrasadoMultinomial$sem, 
                       z = AtrasadoMultinomial$`nao chegou atrasado`,
                       type = "mesh3d",
                       name = "nao chegou atrasado") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

atrasadoprimeira <- plot_ly(x = AtrasadoMultinomial$dist, 
                            y = AtrasadoMultinomial$sem, 
                            z = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
                            type = "mesh3d",
                            name = "chegou atrasado à primeira aula") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

atrasadosegunda <- plot_ly(x = AtrasadoMultinomial$dist,
                           y = AtrasadoMultinomial$sem,
                           z = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
                           type = "mesh3d",
                           name = "chegou atrasado à segunda aula") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

subplot(naoatrasado, atrasadoprimeira, atrasadosegunda)


######################################FIM#####################################
