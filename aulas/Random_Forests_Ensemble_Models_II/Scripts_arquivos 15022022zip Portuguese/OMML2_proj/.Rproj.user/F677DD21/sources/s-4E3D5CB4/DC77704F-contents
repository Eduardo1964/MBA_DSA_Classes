#########################################
# Árvore com variável resposta contínua #
# Esse código é continuação do script01 #
#########################################

#####
# Gerando os dados
# x é uma sequencia de valores entre 0 e 1
x <- seq(0,1, length.out=1000)

# y segue uma relação quadrática
a <- 0
b <- 10
c <- -10

set.seed(2360873)
y <- a + b*x + c*x**2 + rnorm(length(x), mean=0, sd=.1)

df <- data.frame(x, y)

p0 <- ggplot(df, aes(x,y)) + 
  geom_point(aes(colour='Observado')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.85, name = "Valor") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0, 'cm'))
p0

########################
# Construindo a árvore #
tree <- rpart(y~x, 
              data=df,
              control=rpart.control(maxdepth = 2, cp=0))

# Plotando a árvore
paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(tree,
                       box.palette = paleta) # Paleta de cores

# Valores preditos
df['p'] = predict(tree, df)
df['r'] = df$y - df$p

# Valores esperados e observados
boost0_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

boost0_O_vs_E

# Gráfico de resíduos
boost0_res <- ggplot(df, aes(x,r)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(title="Gráfico de resíduos") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")
boost0_res

ggpubr::ggarrange(boost0_O_vs_E, boost0_res, 
                  # labels = c("A", "B"),
                  ncol = 2, nrow = 1)

# Primeira iteração de boosting manual
tree1 <- rpart(r~x, 
               data=df,
               control=rpart.control(maxdepth = 2, cp=0))

df['p1'] = predict(tree1, df)  # Predito da árvore neste passo
df['P1'] = df$p + df$p1        # Predito do boosting (acumulado)
df['r1'] = df$r - df$p1        # resíduo do boosting


# Gráfico da primeira iteração de boosting manual
# O QUE O MODELO ESTÁ FAZENDO
boost1_r_vs_E <- ggplot(df, aes(x,r)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p1, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Variável resposta neste passo") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")


# O QUE ACONTECE COM O MODELO FINAL
boost1_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,P1, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Observado vs Esperado (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

# Gráfico de resíduos
boost1_r <- ggplot(df, aes(x,r1)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Gráfico de resíduos (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")

ggpubr::ggarrange(boost1_r_vs_E, boost1_O_vs_E, boost1_r,
                  # labels = c("A", "B"),
                  ncol = 3, nrow = 1)

#####
# Terceira iteração do boosting

tree2 <- rpart(r1~x, 
               data=df,
               control=rpart.control(maxdepth = 2, cp=0))

df['p2'] = predict(tree2, df) # predito da árvore tree2
df['P2'] = df$P1 + df$p2      # predito do boosting neste passo
df['r2'] = df$r1 - df$p2      # resíduo da árvore neste passo (resíduo do boosting)
# df['r2'] = df$y - df$P2     # O mesmo que a linha acima


# Gráfico da primeira iteração de boosting manual
# O QUE O MODELO ESTÁ FAZENDO
boost2_r_vs_E <- ggplot(df, aes(x,r1)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p2, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Variável resposta neste passo") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y(i)") +
  scale_x_continuous(name= "x")


# O QUE ACONTECE COM O MODELO FINAL
boost2_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,P2, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Observado vs Esperado (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

# Gráfico de resíduos
boost2_r <- ggplot(df, aes(x,r2)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Gráfico de resíduos (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")

boost2_O_vs_E

ggpubr::ggarrange(boost2_r_vs_E, boost2_O_vs_E, boost2_r,
                  # labels = c("A", "B"),
                  ncol = 3, nrow = 1)
