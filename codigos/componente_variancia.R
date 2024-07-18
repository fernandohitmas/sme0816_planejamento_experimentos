#install.packages('nlme')
library(nlme)
library(ggplot2)
library(lattice)

set.seed(8556609)

# Input dos dados

observacoes <- c(24, 20, 19, 24, 24, 
           17, 24, 30, 27, 36,
           18, 38, 26, 27, 21,
           26, 31, 26, 23, 22,
           22, 30, 20, 29, 31)
batches <- rep(
    as.factor(c("batch 1", "batch 2", "batch 3", "batch 4", "batch 5")),
    each=5)
operators <- rep(
    as.factor(c("opt 1", "opt 2", "opt 3", "opt 4", "opt 5")),
    times=5)

forms <- c("A", "B", "C", "D", "E")
formulacoes <- c()
for (i in c(1:5)) {
    print(teste)
    formulacoes <- c(formulacoes, forms)
    first_el <- forms[1]
    forms <- forms[-1]
    forms <- c(forms, first_el)
}

dados <- data.frame(
    batch=batches,
    operator=operators,
    formulacao=formulacao,
    obs=observacoes
)

ggplot(dados, aes(x=operator, y=obs)) + 
    geom_boxplot()

ggplot(dados, aes(x=batch, y=obs)) + 
    geom_boxplot()
ggplot(dados, aes(x=operator, y=obs)) + 
    geom_point() + geom_line()+
    facet_grid(col=vars(batch))

xyplot(
    obs ~ operator| batch, data = dados, type=c("l","g"),
    scales=list(x=list(rot=30)))

mod1 <- aov(obs ~ batch + operator, dados)
summary(mod1)
res <- rstudent(mod1)


# Normalidade
par(mfrow=c(1,2),pty="s")
qqnorm(res,col="blue")
qqline(res,col="blue")
boxplot(res)

# Homogeneidade de variâncias
par(mfrow=c(1,2))
require(lattice)
dotplot(dados$obs ~ dados$batch)
dotplot(dados$obs ~ dados$operator)

# Homogeneidade de variâncias
par(mfrow=c(1,1))
plot(mod1$fit,res,xlab="y.hat")
abline(h=0)
abline(h=-2, lty=2, c="red")
abline(h=2, lty=2)

