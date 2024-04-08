# Duas máquinas são utilizadas para envasar um líquido em frascos de
# plástico. Com o objetivo de verificar se há diferença entre os
# volumes médios envasados, duas amostras de 12 e 10 frascos foram
# selecionadas. Os volumes (em ml) foram medidos resultando nos
# seguintes valores

# Dados
maquina1 <- c(30.9, 30.9, 30.8, 30.7, 30.9, 30.6, 30.8, 30.9, 30.7, 30.9, 30.7,31.0)
maquina2 <- c(30.8, 30.9, 30.7, 30.5, 30.5, 30.6, 30.7, 30.3, 30.6, 30.7)

# alpha
alpha <- 0.05
 
# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(maquina1, horizontal=TRUE,  xaxt="n", frame=F )#, horizontal=TRUE , ylim=c(-10,20),, col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(maquina1) #, breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(maquina2, horizontal=TRUE,  xaxt="n" )#, horizontal=TRUE , ylim=c(-10,20),, col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(maquina2) #, breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(c(maquina1, maquina2), horizontal=TRUE,  xaxt="n" )#, horizontal=TRUE , ylim=c(-10,20),, col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(c(maquina1, maquina2)) #, breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))

# Quer-se saber se a média das duas amostras é a mesma
# H0: mu1 = mu2 x H1: mu1 

