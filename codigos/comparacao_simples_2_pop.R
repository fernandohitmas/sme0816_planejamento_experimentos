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

qt(0.025, 20-4)


samp1 <- rnorm(10, 0, 1)
samp2 <- rnorm(10, 0, 1)

f.test(samp1, samp2)

boxplot(y ~ trat, dados)

# Removendo todos os objetos
rm(list=ls(all=TRUE))
# Entrada dos dados para análise
y<- c( 2, 2, 1, 1, 0,
 1, 0, 0, 1, 1,
 12, 10, 14, 17, 11,
 7, 9, 15, 8, 10)
trat<- rep(c("A","B","C","D"), each=5)
(dados<- data.frame(trat, y))
# Análise de variância
modelo<-lm(y ~ trat, dados)
# 1. Obtenção dos resíduos e análise
# exploratória dos resíduos
res <- residuals(modelo)# resíduos simples
res_Stud <- rstandard(modelo)# resíduos
# Studentizados
round(data.frame(res,res_Stud),5)
boxplot(res_Stud)
# 2. Normalidade dos erros
# 2.1 Análise gráfica dos resíduos - Gráfico quantil-quantil
qqnorm(res_Stud,size=5,col="#000004",cex=0.7,xlab="Quantis da distribuição normal",ylab="Resíduos Studentizados")
# 2.2 Teste de normalidade de Shapiro-Wilk
shapiro.test(res_Stud)
# 3. Homogeneidade de variâncias
# 3.1 Análise gráfica dos resíduos
library(lattice)
dotplot(res_Stud~trat,ylab="Resíduos Studentizados", xlab="Variedade")
boxplot(res_Stud~trat,ylab="Resíduos
 Studentizados",xlab="Variedade")
# 3.2 Teste de # 3.2 Teste de Breusch-Pagan
library(lmtest)
bptest(modelo)
# 4. Verificando a necessidade de setransformar
# os dados
# 4.1 Análise gráfica dos resíduos
plot(res_Stud~fitted(modelo),ylab="Resíduos Studentizados",xlab="Valores esperados (médias)")
abline(h=0)
# 4.2 Transformação de Box-Cox
library(MASS)
boxcox(y+0.001 ~ trat,ylab="logaritmo da verossimilhança")
# transformação sugerida - > y^0.5
dados$yt<- (y+0.01)^0.5
modelot<- lm(yt ~ trat, dados)
qqnorm(rstandard(modelot),col="blue",cex=0.7,
xlab="Quantis
 da distribuição normal",ylab="Resíduos
 Studentizados")
shapiro.test(rstandard(modelot))
dotplot(rstandard(modelot)~trat,ylab="Resíduos
 Studentizados",
 xlab="Variedade")
bptest(modelot)
plot(rstandard(modelot)~fitted(modelot),ylab="
Resíduos
 Studentizados",xlab="Valores esperados
 (médias)")
abline(h=0)
boxcox(modelot,ylab="logaritmo da
 verossimilhança")
anova(modelot)
library(ExpDes.pt)
dic(dados$trat, dados$yt)
#apresentação das médias
round((tapply(dados$yt, dados$trat, mean))^2-
0.01,4)
