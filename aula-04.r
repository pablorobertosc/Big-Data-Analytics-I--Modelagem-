##############################################################
##BDA - AULA 4
##############################################################
##Exemplo 1    
##############################################################
altura<-c(1.98,1.97,2.10,2.05,2.00,1.85)
xbarra<-round(mean(altura),2)
cat("Média aritmética = ",xbarra, "\n")

##############################################################
##Exemplo 2    
##############################################################
xi<-c(1,2,3,4,5,6)
fi<-c(1,3,3,10,15,8)
n<-sum(fi)
fi.xi<-fi*xi

xbarra<-round(sum(fi.xi)/n,1)
cat("Média aritmética = ",xbarra, "\n")

##############################################################
##Exemplo 3    
##############################################################
library(MASS)

class(ships)
head(ships)
View(ships)
str(ships)
which(is.na(ships))

xbarra<-mean(ships$incidents)
cat("Média aritmética = ",xbarra, "\n")

s.tipo<-subset(ships, type == "A", select = incidents)
s.tipo
class(s.tipo)

cat("Média aritmética para tipo A = ", mean(s.tipo$incidents), "\n")

médias<-tapply(ships$incidents, ships$type, mean)

incidentes.tipo <- tapply(ships$incidents, ships$type, sum)

barplot(incidentes.tipo,
        main = "Número de incidentes por tipo de navio",
        xlab = "tipo de navio",
        ylab = "total de incidentes",
        col = "turquoise")

##############################################################
##Exemplo 4    
##############################################################
k<-5
h<-5
L0<-29
cons<-seq(L0,L0+h*k,h)
fi<-c(11,12,2,3,2)
n<-sum(fi)
xi<-NULL

#calcula os valores de xi
for(i in 1:k) xi[i]<-(cons[i] + cons[i+1])/2
fi.xi<-fi*xi
xbarra<-sum(fi.xi)/n
cat("Média aritmética = ",xbarra, "\n")

##############################################################
##Exemplo 5    
##############################################################
k<-6
h<-4
L0<-150
estatura<-seq(L0,L0+h*k,h)
fi<-c(4,9,11,8,5,3)
n<-sum(fi)

xi<-NULL
#calcula os valores de xi
for(i in 1:k) xi[i]<-(estatura[i] + estatura[i+1])/2
fi.xi<-fi*xi
xbarra<-sum(fi.xi)/n
#calcula em metros
xbarra<-round(xbarra/100,2)
cat("Média aritmética = ",xbarra, "\n")

##############################################################
##Exemplo 6    
##############################################################
class(pressure)
head(pressure)
View(pressure)
str(pressure)
which(is.na(pressure))

names(pressure)<-c("t","p")
View(pressure)

xbarra<-round(mean(pressure$t),2)
cat("Média aritmética = ",xbarra, "\n")

s.temp<-subset(pressure, t >= 23 & t <= 72, select = p)

cat("Média aritmética da pressão para 23°C <= t <= 72°C =",mean(s.temp$p),"mmHg\n")

plot(pressure$t, pressure$p, main = "Relação entre temperatura e pressão", 
     xlab = "temperatura (°C)", 
     ylab = "pressão (mmHg)", col = "firebrick2", pch = 19)



