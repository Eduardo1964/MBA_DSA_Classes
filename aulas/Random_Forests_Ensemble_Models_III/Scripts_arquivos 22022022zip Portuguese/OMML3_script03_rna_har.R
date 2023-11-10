#####################################
# Rodar uma random forest inicial

levels(HAR_train$y) <- c("andando",  "subindo", "descendo", "sentado",  "em_pe", "deitado")
levels(HAR_test$y) <- c("andando",  "subindo", "descendo", "sentado",  "em_pe", "deitado")

tempo_ini <- Sys.time()
rf <- randomForest::randomForest(y ~ ., 
                   data=HAR_train[,2:563],
                   ntree=20,
                   mtry=100)

rf

tempo_fim <- Sys.time()
tempo_fim - tempo_ini

c_test <- predict(rf, HAR_test)
c_test[1:10]

######################
# Função para avaliar o modelo

avalia <- function(modelo, nome_modelo="modelo", df_treino, df_teste, vresp = "y"){
  p_treino <- predict(modelo, df_treino, type='prob') # Probabilidade predita
  c_treino <- predict(modelo, df_treino)              # Classificação
  
  #Base de teste
  p_teste <- predict(modelo, df_teste, type='prob')
  c_teste <- predict(modelo, df_teste)
  
  cm_treino <- confusionMatrix(c_treino, df_treino[,vresp])
  print(cm_treino$table)
  print(cm_treino$overall["Accuracy"])
  
  cm_teste <- confusionMatrix(c_teste,  df_teste[,vresp])
  print(cm_teste$table)
  print(cm_teste$overall['Accuracy'])
  
}

avalia(rf, nome_modelo="rf", HAR_train, HAR_test, vresp = "y")
#######################

rf$importance

variaveis_selecionadas <- rf$importance %>% as.data.frame() %>% top_n(6) %>% row.names

variaveis_selecionadas

HAR_train$y %>% table

nnGrid <-  expand.grid(.size=6, .decay = c(0))
# nnGrid <-  expand.grid(.size=7, .decay = c(0, .005, .010, 0.015))
ctrl <- caret::trainControl(method='cv', number=4, classProbs=TRUE)

nnfit <- caret::train(y ~ .,
                      data=HAR_train[,c(variaveis_selecionadas, 'y')],
                      method='nnet',
                      metric='Accuracy',
                      tuneGrid=nnGrid,
                      trControl=ctrl,
                      maxit=100,
                      verboseIter = FALSE
)

# Resultados do grid search
nnfit$results

avalia(nnfit, nome_modelo="rf", HAR_train, HAR_test, vresp = "y")
