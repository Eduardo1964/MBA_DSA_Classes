# carregar os dados
load(file='EPA_19.RData')

# checar a estrutura
df %>% str

# colunas quantitativas para padronizar entre 0 e 1
cols <- c("fuel_economy_combined", 'eng_disp', 'num_cyl', 'num_gears', 'batt_capacity_ah')

# função para padronizar
range01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# padronizar variaveis quantitativas
df[cols] <- lapply(df[cols], range01)

df %>% head


##################### 
# Criar uma matriz de dados com somente numéricos

# criar a fórmula tipo y ~ x1 + x2 ... + xn
n <- names(df)

f_variaveis <- paste(n[-1], collapse = " + ")
f_variaveis

f <- as.formula(paste(n[1], " ~ ", f_variaveis))
f

df %>% str

m <- model.matrix(f, data = df)
m <- as.matrix(data.frame(m, df[, 1]))

#######################################
# Criar uma fórmula para a matriz m
colnames(m)[28] <- "fuel_economy_combined"

nomes <- colnames(m)
f_variaveis <- paste(nomes[-28], collapse=' + ')

f <- as.formula(paste(nomes[28], " ~ ", f_variaveis))
f

# Treinando um Linear Perceptron
set.seed(1729)
start_time <- Sys.time() # Marca o tempo de início
nn <- neuralnet(f, # Fórmula
                data=m, # dados
                linear.output = TRUE # indica resposta contínua
                )
end_time <- Sys.time() # Marca o tempo de fim
t <- end_time - start_time # Mostra o tempo de execução
t

plot(nn)

pred <- predict(nn, m)
plot(x = pred, y = df$fuel_economy_combined)
caret::postResample(pred, df$fuel_economy_combined)

N <- nrow(m)

################################################
# Vamos fazer uma rede com camadas escondidas

start_time <- Sys.time() # Marca o tempo de início
nn <- neuralnet(f, data=m, hidden = c(7, 3), linear.output = TRUE)
end_time <- Sys.time() # Marca o tempo de fim
t <- end_time - start_time # Mostra o tempo de execução
t

pred <- predict(nn, m)
plot(x = pred, y = df$fuel_economy_combined)
caret::postResample(pred, df$fuel_economy_combined)


###################################################### 
# Agora vamos fazer um k-fold para avaliar o modelo

# montando um k-fold
k <- 10 #número de folds
stats <- NULL # Inicializando a qualidade dos modelos do fold
m2 = m[sample(1:nrow(m)), ] # m2 é uma permutação de m para fazermos os folds

for (i in 1:(k-1)){
  ind_treino <- !seq(N)>N*(i/k) & seq(N)<=N*((i+1)/k)
  ind_teste <- seq(N)>N*(i/k) & seq(N)<=N*((i+1)/k)
  
  nn <- neuralnet(f, data=m2[ind_treino,], hidden = c(7, 3), linear.output = TRUE)
  pred <- predict(nn, m[ind_teste,])
  stats_tmp <- caret::postResample(pred, df$fuel_economy_combined[ind_teste])
  stats <- rbind(stats, stats_tmp)

}

stats %>% colMeans()


plot(nn)

pred <- predict(nn, m)
plot(x = pred, y = df$fuel_economy_combined)
caret::postResample(pred, df$fuel_economy_combined)

##############################
# Tunando a árvore

# CV Grid Search para 'tunar' o parâmetro decay
nnGrid <-  expand.grid(.size=7, .decay = c(0, .005, .01, 0.015, 0.02, 0.025))
?expand.grid
ctrl <- caret::trainControl(method='cv')

nnfit <- caret::train(f,
                      data=m2,
                      method='nnet',
                      tuneGrid=nnGrid,
                      trControl=ctrl,
                      maxit=1000,
                      verboseIter = FALSE
                      )

nnfit$results

modelo.final <- nnfit$finalModel

pred <- predict(modelo.final, m)
plot(x = pred, y = df$fuel_economy_combined)
caret::postResample(pred, df$fuel_economy_combined)

