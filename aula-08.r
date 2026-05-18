##############################################################
##BDA - AULA 8
##############################################################
##Exemplo 1    
##############################################################
A<-c(41,38,39,45,47,41,44,41,37,42)
B<-c(40,23,41,50,49,32,41,29,52,58)

AT.A<-max(A) - min(A)
AT.B<-max(B) - min(B)

cat("AT(A) = ", AT.A*100, "\n")
cat("AT(B) = ", AT.B*100, "\n")

xbarra.A<-mean(A)
n<-length(A)

##############################################################
##CRIANDO UMA FUNÇÃO EM R
##############################################################
DesvMA<-function(dados,média,tam){
  return(sum(abs(dados - média))/tam)
}
##############################################################

DMA.A<-DesvMA(A,xbarra.A,n)

xbarra.B<-mean(B)
m<-length(B)
DMA.B<-DesvMA(B,xbarra.B,m)

cat("DMA(A) = ", round(DMA.A*100,2), "\n")
cat("DMA(B) = ", round(DMA.B*100,2), "\n")

s2.A<-var(A)
s.A<-sd(A)

s2.B<-var(B)
s.B<-sd(B)

cat("s2(A) = ", round(s2.A*100,2), "\n")
cat("s(A) = ", round(s.A*100,2), "\n")

cat("s2(B) = ", round(s2.B*100,2), "\n")
cat("s(B) = ", round(s.B*100,2), "\n")

CV.A<-(s.A/xbarra.A)*100
cat("CV(A) = ", round(CV.A,1), "% \n")

CV.B<-(s.B/xbarra.B)*100
cat("CV(B) = ", round(CV.B,1), "% \n")

res<-data.frame(AT.A*100, round(DMA.A*100,2), round(s2.A*100,2), 
                round(s.A*100,2), round(CV.A,1))
res[2,]<-c(AT.B*100, round(DMA.B*100,2), round(s2.B*100,2), 
           round(s.B*100,2), round(CV.B,1))
names(res)<-c("AT","DMA","s^2","s","CV(%)")
row.names(res)<-c("Empresa A","Empresa B")

##############################################################
##Exemplo 2    
##############################################################
xI<-c(1,2,3,4,5,6,7)
fI<-c(6,8,7,5,5,4,2)
tipoI<-rep(xI,fI)

xII<-c(1,2,3,4,5,6,7)
fII<-c(3,2,4,6,8,1,4)
tipoII<-rep(xII,fII)

AT.I<-max(tipoI) - min(tipoI)
AT.II<-max(tipoII) - min(tipoII)

cat("AT(I) = ", AT.I, "\n")
cat("AT(II) = ", AT.II, "\n")

xbarra.I<-mean(tipoI)
n<-length(tipoI)
DMA.I<-DesvMA(tipoI,xbarra.I,n)

xbarra.II<-mean(tipoII)
m<-length(tipoII)
DMA.II<-DesvMA(tipoII,xbarra.II,m)

cat("DMA(I) = ", round(DMA.I,2), "\n")
cat("DMA(II) = ", round(DMA.II,2), "\n")

s2.I<-var(tipoI)
s.I<-sd(tipoI)

s2.II<-var(tipoII)
s.II<-sd(tipoII)

cat("s2(I) = ", round(s2.I,2), "\n")
cat("s(I) = ", round(s.I,2), "\n")

cat("s2(II) = ", round(s2.II,2), "\n")
cat("s(II) = ", round(s.II,2), "\n")

CV.I<-(s.I/xbarra.I)*100
cat("CV(I) = ", round(CV.I,2), "% \n")

CV.II<-(s.II/xbarra.II)*100
cat("CV(II) = ", round(CV.II,2), "% \n")

res<-data.frame(AT.I, round(DMA.I,2), round(s2.I,2), 
                round(s.I,2), round(CV.I,2))
res[2,]<-c(AT.II, round(DMA.II,2), round(s2.II,2), 
           round(s.II,2), round(CV.II,2))
names(res)<-c("AT","DMA","s^2","s","CV(%)")
row.names(res)<-c("Tipo I","Tipo II")
t(res)

plot(xI,fI, type = "h", main = "Histograma (Tipo I)",
     xlab = "número de exigências descumpridas",
     ylab = "número de propostas",lwd = 5, col = "forestgreen",
     ylim = c(0,8))
points(xbarra.I, 0, col = "red", pch = 20)
points(c(xbarra.I - s.I, xbarra.I + s.I), c(0,0), col = "black", pch = 4)

plot(xII,fII, type = "h", main = "Histograma (Tipo II)",
     xlab = "número de exigências descumpridas",
     ylab = "número de propostas",lwd = 5, col = "palegreen",
     ylim = c(0,8))
points(xbarra.II, 0, col = "red", pch = 20)
points(c(xbarra.II - s.II, xbarra.II + s.II), c(0,0), col = "black", pch = 4)

##############################################################
##Exemplos 3,4 e 5    
##############################################################
library(ebda)

k<-7
h<-500
L0<-1000
fi<-c(8,10,11,16,13,5,2)

tab<-tab.df(k,h,L0,fi)

xbarra<-m.a(tab)
cat("xbarra  = ",round(xbarra,2), "\n")

#m.dma: Função do pacote ebda que calcula o DMA
DMA<-m.dma(tab)
cat("DMA = ", round(DMA,2), " \n")

#m.var: Função do pacote ebda que calcula a variância
s2<-m.var(tab)
cat("s2 = ", round(s2,2), " \n")

s<-sqrt(s2)
cat("s = ", round(s,2), " \n")

AT<-max(tab[k,5]) - min(tab[1,5])
cat("AT = ", round(AT,2), " \n")
CV<-(s/xbarra)*100
cat("CV = ", round(CV,2), "% \n")

salário<-rep(tab[,5],fi)
hist(salário,main = "Histograma", xlab = "salário mensal (R$)", 
     ylab = "número de funcionários", col = "springgreen",
     ylim = c(0,18)) 
points(xbarra, 0, col = "red", pch = 20, cex = 1.5)
points(c(xbarra - s, xbarra + s), c(0,0), col = "black", pch = 4)

##############################################################
##Exemplo 6    
##############################################################
anorexia<-read.csv("anorexia.csv", header = TRUE, sep = ",", dec = "." )
str(anorexia)
head(anorexia)
View(anorexia)
which(is.na(anorexia))

anorexia$Prewt<-anorexia$Prewt*0.453592
anorexia$Postwt<-anorexia$Postwt*0.453592


média.pre<-mean(anorexia$Prewt)
média.pos<-mean(anorexia$Postwt)

DMA.pre<-DesvMA(anorexia$Prewt,média.pre,length(anorexia$Prewt))
DMA.pos<-DesvMA(anorexia$Postwt,média.pos,length(anorexia$Postwt))

s2.pre<-var(anorexia$Prewt)
s2.pos<-var(anorexia$Postwt)

s.pre<-sd(anorexia$Prewt)
s.pos<-sd(anorexia$Postwt)

AT.pre<-max(anorexia$Prewt) - min(anorexia$Prewt)
AT.pos<-max(anorexia$Postwt) - min(anorexia$Postwt)

CV.pre<-(s.pre/média.pre)*100
CV.pos<-(s.pos/média.pos)*100

tabela.res<-matrix(round(c(média.pre,DMA.pre,s2.pre,s.pre, AT.pre, CV.pre, 
                           média.pos,DMA.pos, s2.pos, s.pos, AT.pos, CV.pos),1),
                   byrow = TRUE, nrow = 2, ncol = 6) 

rownames(tabela.res)<-c("Antes do período de estudo",
                        "Após do período de estudo")
colnames(tabela.res)<-c("média", "DMA", "s2","s","AT","CV(%)")

tabela.res

tabela.res1<-t(tabela.res)
tabela.res1

##mfrow: MultiFrame ROWwise layout
opar<-par(mfrow = c(1,2))

hist(anorexia$Prewt, col = "lightblue", main = "Histograma",  
     xlab = "peso (kg) antes o período de estudo", 
     ylab = "Número de pacientes do gênero feminino",
     xlim = c(30,50))
points(média.pre, 0, col = "red", pch = 20, cex = 1.5)
points(c(média.pre - s.pre, média.pre + s.pre), c(0,0), col = "black", pch = 4)


hist(anorexia$Postwt, col = "royalblue", main = "Histograma",  
     xlab = "peso (kg) após o período de estudo", 
     ylab = "Número de pacientes do gênero feminino",
     xlim = c(30,50))
points(média.pos, 0, col = "red", pch = 20, cex = 1.5)
points(c(média.pos - s.pos, média.pos + s.pos), c(0,0), col = "black", pch = 4)

par(opar)

