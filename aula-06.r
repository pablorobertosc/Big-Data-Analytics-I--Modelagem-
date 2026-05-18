

##############################################################
##BDA - AULA 6
##############################################################
##Exemplo 1    
##############################################################
library(ebda)

proj<-c(6, 6, 6, 9, 9, 9, 12, 12, 12, 12, 15, 15)
cat("Moda  = ",moda(proj), "\n")

##############################################################
##Exemplo 2    
##############################################################
xi<-c(0,1,2,3,4,5,6)
fi<-c(12,15,10,10,20,18,15)
n.vezes<-rep(xi,fi)
cat("Moda  = ",moda(n.vezes), "\n")

##############################################################
##Exemplo 3    
##############################################################
k<-6
h<-4
L0<-150
fi<-c(4,9,11,8,5,3)

tab<-tab.df(k,h,L0,fi)
cat("Moda  = ",round(Mo(tab)/100,2), "\n")

##############################################################
##Exemplo 4    
##############################################################
artrite<-read.csv("Arthritis.csv", header = TRUE, sep = ";", dec = ".")

class(artrite)
tail(artrite)
View(artrite)
which(is.na(artrite))
str(artrite)

artrite$Improved<-factor(artrite$Improved, 
                         levels = c("None", "Some", "Marked"), 
                                    ordered = TRUE)

#Dentre os pacientes que receberam tratamento qual foi o desfecho mais frequente?
trat<-subset(artrite, artrite$Treatment == "Treated")
frequências<-table(trat$Improved)
nomes<-names(frequências)
f.moda <- nomes[frequências == max(frequências)]
cat("Moda  = ",f.moda, "\n")

#Para os pacientes do sexo masculino que receberam tratamento, qual o desfecho mais 
#frequente?E para o sexo feminino?
trat<-subset(artrite, artrite$Treatment == "Treated", select = c(Sex,Improved))
frequências<-table(trat$Sex,trat$Improved)

moda.sexo <- apply(frequências, 1, function(frequências) {
  nomes <- names(frequências)
  index <- which(frequências == max(frequências))
  nomes[index]
})


tab<-table(trat$Improved, trat$Sex)
tab
barplot(tab, main = "Treatment outcome by sex ",
        xlab = "Sex",
        ylab = "Frequency", ylim = c(0,20),
        col = c("palegreen", "green","olivedrab"),
        legend.text = rownames(tab),
        args.legend = list(x = "topright",
                           bty = "n",
                           cex = 0.8),
        beside = TRUE)  

