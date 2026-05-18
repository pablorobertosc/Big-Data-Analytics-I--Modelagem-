##############################################################
##BDA - AULA 10
##############################################################
##Exemplo 1    
##############################################################
K<-function(x){
  return((quantile(x,0.75, type = 2) - quantile(x,0.25, type = 2))/
           (2*(quantile(x,0.90, type = 2) - quantile(x,0.10, type = 2))))
}

taxa<-c(2.50,2.52,2.53,2.54,2.59,2.60,2.61,2.62,2.64,2.65)
K.taxa<-K(taxa)
cat("curtose = ", round(K.taxa,3))

qqnorm(taxa, main = "Gráfico Q - Q", 
       xlab = "quantis teóricos", ylab = "quantis amostrais", 
       col = "purple",pch = 16)
qqline(taxa, col = "purple", lwd = 2)
text(-1.2, 2.60, "K ~ ", cex = 0.8)
text(-1.0, 2.60, round(K.taxa,3), cex = 0.8)

##############################################################
##Exemplo 2    
##############################################################
medida<-function(x,i){
  return(boxplot.stats(x)$stats[i])
}

As<-function(x){
  return((3*(mean(x) - median(x)))/sd(x))
}

fivenum(taxa)

LI<-medida(taxa,1)
cat("LI  = ",round(LI,4), "\n")
Q1<-medida(taxa,2)
cat("Q1  = ",round(Q1,4), "\n")
Md<-medida(taxa,3)
cat("Md  = ",round(Md,4), "\n")
Q3<-medida(taxa,4)
cat("Q3  = ",round(Q3,4), "\n")
LS<-medida(taxa,5)
cat("LS  = ",round(LS,4), "\n")
dQ<-Q3 - Q1
cat("dQ  = ",round(dQ,4), "\n")

boxplot(taxa, horizontal = TRUE, main = "Box plot",
        col = "violet", xlab = "taxas de juros (%)")
stripchart(c(Q1,Md,Q3), vertical = FALSE, add = TRUE, 
           method = "stack", col = "red", pch = 20)
stripchart(c(LI, LS), vertical = FALSE, add = TRUE, 
           method = "stack", col = "blue", pch = 20)

As.taxa<-As(taxa)
cat("As = ", round(As.taxa,2),"\n")
K.ação<-K(taxa)
cat("K = ", round(K.taxa,3),"\n")

opar<-par(mfrow = c(2,2))

plot(density(taxa), xlab = "taxas de juros (%)", 
     ylab = "densidade", col = "darkblue", main = "Assimetria")
text(2.42, 5, "As ~ ", cex = 0.8)
text(2.45, 5, round(As.taxa,2), cex = 0.8)
points(mean(taxa), 0, col = "red", pch = 20)
arrows(mean(taxa), 0, mean(taxa), 6, code = 1, length = 0.15,
       col = "red", lty = 2)
points(median(taxa), 0, col = "orange", pch = 17)
arrows(median(taxa), 0, median(taxa), 6, code = 1, length = 0.15,
       col = "orange", lty = 2)

boxplot(taxa, col = "violet", horizontal = TRUE, 
        xlab = "taxa de juros (%)", main = "Box plot")

qqnorm(taxa, main = "Gráfico Q - Q (Curtose)", 
       xlab = "quantis teóricos", ylab = "quantis amostrais", 
       col = "purple",pch = 16)
qqline(taxa, col = "black", lwd = 2)
text(-1.2, 2.60, "K ~ ", cex = 0.8)
text(-1.0, 2.60, round(K.taxa,3), cex = 0.8)

hist(taxa, col = "violet",xlab = "taxa de juros (%)", 
     ylab = "Frequência absoluta", main = "Histograma",
     breaks = "Sturges")

par(opar)

##############################################################
##Exemplo 3    
##############################################################
pub<-read.csv("Publicidade.csv", header = TRUE, sep = ";", dec = "," )
str(pub)
head(pub)
View(pub)
which(is.na(pub))

fivenum(pub$vendas)

LI<-medida(pub$vendas,1)
cat("LI  = ",round(LI,4), "\n")
Q1<-medida(pub$vendas,2)
cat("Q1  = ",round(Q1,4), "\n")
Md<-medida(pub$vendas,3)
cat("Md  = ",round(Md,4), "\n")
Q3<-medida(pub$vendas,4)
cat("Q3  = ",round(Q3,4), "\n")
LS<-medida(pub$vendas,5)
cat("LS  = ",round(LS,4), "\n")
dQ<-Q3 - Q1
cat("dQ  = ",round(dQ,4), "\n")

boxplot(pub$vendas, horizontal = TRUE, main = "Box plot",
        col = "slateblue4", xlab = "Vendas (em milhares de unidades)")
stripchart(c(Q1,Md,Q3), vertical = FALSE, add = TRUE, 
           method = "stack", col = "red", pch = 20)
stripchart(c(LI, LS), vertical = FALSE, add = TRUE, 
           method = "stack", col = "blue", pch = 20)

As.vendas<-As(pub$vendas)
cat("As = ", round(As.vendas,2),"\n")
K.vendas<-K(pub$vendas)
cat("K = ", round(K.vendas,3),"\n")

opar<-par(mfrow = c(2,2))

plot(density(pub$vendas), xlab = "Vendas (em milhares de unidades)", 
     ylab = "densidade", col = "slateblue1", main = "Assimetria")
text(2.0, 0.08, "As ~ ", cex = 0.8)
text(4.0, 0.08, round(As.vendas,2), cex = 0.8)
points(mean(pub$vendas), 0, col = "red", pch = 20)
arrows(mean(pub$vendas), 0, mean(pub$vendas), 0.08, code = 1, length = 0.15,
       col = "red", lty = 2)
points(median(pub$vendas), 0, col = "orange", pch = 17)
arrows(median(pub$vendas), 0, median(pub$vendas), 0.08, code = 1, length = 0.15,
       col = "orange", lty = 2)

boxplot(pub$vendas, col = "slateblue4", horizontal = TRUE, 
        xlab = "Vendas (em milhares de unidades)", main = "Box plot")

qqnorm(pub$vendas, main = "Gráfico Q - Q (Curtose)", 
       xlab = "quantis teóricos", ylab = "quantis amostrais", 
       col = "slateblue1",pch = 16)
qqline(pub$vendas, col = "black", lwd = 2)
text(-2, 25, "K ~ ", cex = 0.8)
text(-1.5, 25, round(K.vendas,3), cex = 0.8)

hist(pub$vendas, col = "slateblue4",xlab = "Vendas (em milhares de unidades)", 
     ylab = "Frequência absoluta", main = "Histograma",
     breaks = "Sturges")

par(opar)

##############################################################
##Exemplo 4    
##############################################################
library(ebda)

k<-5
h<-5
L0<-29
fi<-c(2,12,11,3,2)
tab<-tab.df(k,h,L0,fi)
  
K<-m.curtose(tab)
cat("K = ", round(K,3), "\n")

x<-rep(tab[,5],fi)

opar<-par(mfrow = c(1,2))
qqnorm(x, main = "Gráfico Q - Q", 
       xlab = "quantis teóricos", ylab = "quantis amostrais", 
       col = "purple",pch = 16)
qqline(x, col = "violet", lwd = 2)
text(-1.5, 45, "K ~ ", cex = 0.8)
text(-0.8, 45, round(K,3), cex = 0.8)
hist(x, main = c("K",round(K,3)), xlab = "consumo de combustível (mpg)",
     ylab = "densidade", col = "purple", probability = TRUE,
     ylim = c(0,0.1))
lines(density(x), lwd = 2, col = "violet")
box()
par(opar)

