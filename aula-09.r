##############################################################
##BDA - AULA 9
##############################################################
##Exemplo 1    
##############################################################
ação<-c(2.50,2.52,2.53,2.54,2.59,2.60,2.61,2.62,2.64,2.65)
xbarra<-mean(ação)
Md<-median(ação)
s<-sd(ação)
As<-(3*(xbarra - Md))/s
cat("As = ", round(As,2),"\n")

hist(ação, prob = TRUE, xlab = "taxa de juros da ação", 
     ylab = "densidade", col = "green", main = c("As",round(As,2)),
     xlim = c(2.40,2.75))
lines(density(ação), 
      lwd = 2, 
      col = "forestgreen", xlim = c(2.40,2.75))

arrows(xbarra, 0, xbarra, 6, code = 1, length = 0.15,
       col = "red", lty = 2)
arrows(Md, 0, Md, 6, code = 1, length = 0.15,
       col = "blue", lty = 2)
points(xbarra, 0, col = "red", pch = 20, cex = 1.5)
points(Md, 0, col = "blue", pch = 17, cex = 1.5)

#Você também pode usar a função assimetria(x) do pacote edba
library(ebda)
assimetria(ação)

##############################################################
##Exemplo 2    
##############################################################
xi<-c(1,2,3,4,5,6)
fi<-c(1,3,3,10,15,8)

pedidos<-rep(xi,fi)
Md<-median(pedidos)

xbarra<-mean(pedidos)
s<-sd(pedidos)
As<-(3*(xbarra - Md))/s
cat("As = ", round(As,2),"\n")

plot(xi,fi,type = "h", xlab = "número de pedidos", 
     ylab = "frequência absoluta", col = "purple", main = c("As",round(As,2)),
     lwd = 5, ylim = c(0,16))
arrows(xbarra, 0, xbarra, 16, code = 1, length = 0.15,
       col = "red", lty = 2)
arrows(Md, 0, Md, 16, code = 1, length = 0.15,
       col = "orange", lty = 2)
points(xbarra, 0, col = "red", pch = 20, cex = 1.5)
points(Md, 0, col = "orange", pch = 17, cex = 1.5)

##############################################################
##Exemplo 3    
##############################################################
k<-6
h<-10
L0<-10
fi<-c(35,26,17,6,9,2)

tab<-tab.df(k,h,L0,fi)

#A mediana é o segundo quartil, ou seja, i = 2 e m = 4
Md<-m.s(tab,2,4)
xbarra<-m.a(tab)
s2<-m.var(tab)
s<-sqrt(s2)

As<-(3*(xbarra - Md))/s
cat("As = ", round(As,2),"\n")


tempo<-rep(tab[,5],fi)
hist(tempo, prob = TRUE, xlab = "peso", 
     ylab = "densidade", col = "olivedrab", main = c("As",round(As,2)),
     breaks = 6, xlim = c(0,80), ylim = c(0,0.040))
lines(density(tempo), 
      lwd = 2, 
      col = "black", xlim = c(0,80), ylim = c(0,0.040))


arrows(xbarra, 0, xbarra, 0.035, code = 1, length = 0.15,
       col = "red", lty = 2)
arrows(Md, 0, Md, 0.035, code = 1, length = 0.15,
       col = "orange", lty = 2)
points(xbarra, 0, col = "red", pch = 20, cex = 1.5)
points(Md, 0, col = "orange", pch = 17, cex = 1.5)

box()

##############################################################
##Exemplo 4    
##############################################################
cred<-read.csv("Credit.csv", header = TRUE, sep = ";", dec = "," )
str(cred)
head(cred)
View(cred)
which(is.na(cred))

Md.l<-median(cred$limite)
xbarra.l<-mean(cred$limite)
s.l<-sd(cred$limite)
As.l<-(3*(xbarra.l - Md.l))/s.l
cat("As = ", round(As.l,2),"\n")

Md.smd<-median(cred$div_med)
xbarra.smd<-mean(cred$div_med)
s.smd<-sd(cred$div_med)
As.smd<-(3*(xbarra.smd - Md.smd))/s.smd
cat("As = ", round(As.smd,2),"\n")

opar<-par(mfrow = c(1,2))

hist(cred$limite, prob = TRUE, xlab = "Limite no cartão de crédito (U$)", 
     ylab = "densidade", col = "rosybrown", main = c("As",round(As.l,2)),
     xlim = c(-0.5,15000), ylim = c(0,0.0002))
lines(density(cred$limite), 
      lwd = 2, 
      col = "brown", xlim = c(-0.5,15000), ylim = c(0,0.0002))

arrows(xbarra.l, 0, xbarra.l, 0.0002, code = 1, length = 0.15,
       col = "red", lty = 2)
arrows(Md.l, 0, Md.l, 0.0002, code = 1, length = 0.15,
       col = "orange", lty = 2)
points(xbarra.l, 0, col = "red", pch = 20, cex = 1.5)
points(Md.l, 0, col = "orange", pch = 17, cex = 1.5)
box()

hist(cred$div_med, prob = TRUE, xlab = "Saldo médio devedor 
     no cartão de crédito (U$)", 
     ylab = "densidade", col = "chocolate", main = c("As",round(As.smd,2)),
     xlim = c(-0.5,2000), ylim = c(0,0.0020))

lines(density(cred$div_med), 
      lwd = 2, 
      col = "brown", xlim = c(-0.5,2000), ylim = c(0,0.0020))

arrows(xbarra.smd, 0, xbarra.smd, 0.0020, code = 1, length = 0.15,
       col = "red", lty = 2)
arrows(Md.smd, 0, Md.smd, 0.0020, code = 1, length = 0.15,
       col = "orange", lty = 2)
points(xbarra.smd, 0, col = "red", pch = 20, cex = 1.5)
points(Md.smd, 0, col = "orange", pch = 17, cex = 1.5)

box()
par(opar)


