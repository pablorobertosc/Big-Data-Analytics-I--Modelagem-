##BDA - AULA 2
##############################################################
##Exemplos 1 e 2    
##############################################################
filhos<-c(0,1,3,4,2,1,1,2,3,2,2,3,2,2,1,5,3,2,3,2,4,2,2,5,3)
tab.f<-table(filhos)
tab.f
tabr.f<-prop.table(tab.f)
tabr.f
plot(tab.f, main = "Histograma", xlab = "número de filhos",
     ylab = "Frequência", col = "blue", type = "h",
     lwd = 8)
plot(tabr.f, main = "Histograma", xlab = "número de filhos",
     ylab = "Frequência relativa", col = "lightblue", type = "h",
     lwd = 8)

##############################################################
##Exemplos 3 e 4   
##############################################################
erros<-c(9,7,4,3,6,5,8,2,3,6,2,3,0,3,0,2,1,3,1,5,
         11,7,4,2,3,2,4,7,3,2,1,3,2,1,0,1,2,2,2,3,
         3,2,5,4,3,6,2,8,2,3,4,1,2,1,6,1,3,2,1,1)
length(erros)
tab.e<-table(erros)
tab.e
tabr.e<-round(prop.table(tab.e),2)
tabr.e
plot(tab.e, main = "Histograma", xlab = "número de erros",
     ylab = "Frequência", col = "violet", type = "h",
     lwd = 4)
plot(tabr.e, main = "Histograma", xlab = "número de erros",
     ylab = "Frequência relativa", col = "lightpink", type = "h",
     lwd = 4)

##############################################################
##Exemplos 5 e 6    
##############################################################
pluv<-c(15.2,14.6,27.9,24.9,20,43.5,30.7,30,35.7,40.9,
        23.4,17.8,26.9,30.8,19.9,36.8,33.4,19.8,29.6,38.2,
        25.1,42,35.2,15.6,25.5,29.7,27.8,14.6,22.1,24.3,
        30.1,30.1,22.1,24.4,28.7,35,26.1,28.2,19.4,28.7,
        28,25.3,31.8,31,28.3,13.5,32.1,25.4,26.7,36.8)

intervalo<-max(pluv)-min(pluv)
n<-length(pluv)
k<-ceiling(1 + 3.322*log10(n))
h<-ceiling(intervalo/k)
L0<-floor(min(pluv))
classes<-seq(L0,L0+k*h,h)
tab.pluv<-table(cut(pluv,classes))
tab.pluv
tabr.pluv<-round(prop.table(tab.pluv),2)
tabr.pluv

hist(pluv, main = "Histograma", xlab = "pluviosidade anual", 
        ylab = "Frequência Absoluta", col = "salmon",
     breaks = classes)
plot(tabr.pluv, main = "Histograma", xlab = "pluviosidade anual",
     ylab = "Frequência relativa", col = "salmon4", type = "h",
     lwd = 4)

##############################################################
##Exemplos 7 e 8    
##############################################################
tempo<-c(10,21,23,21,10,22,85,51,21,12,12,22,21,12,14,20,10,31,
         23,32,31,32,65,10,18,20,15,13,34,20,35,10,10,10,20,12,
         16,14,43,35,15,15,43,15,68,72,20,18,45,12,22,54,22,21,
         54,86,12,67,18,44,10,32,33,34,25,12,12,12,20,33,45,45,
         33,54,10,52,54,22,66,21,65,11,35,55,23,76,35,54,54,12)
intervalo<-max(tempo)-min(tempo)
n<-length(tempo)
k<-ceiling(1+3.322*log10(n))
h<-ceiling(intervalo/k)
L0<-floor(min(tempo))
classes<-seq(L0,L0+h*k,h)
tab.t<-table(cut(tempo,classes,right = FALSE))
tab.t
tabr.t<-round(prop.table(tab.t),2)
tabr.t

hist(tempo, main = "Histograma", xlab = "tempo de atendimento (min)", 
     ylab = "Frequência Absoluta", col = "springgreen" ,
     breaks = classes)
plot(tabr.t, main = "Histograma", xlab = "tempo de atendimento (min)",
     ylab = "Frequência relativa", col = "springgreen4", type = "h",
     lwd = 4)