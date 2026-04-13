##############################################################
##BDA - AULA 3
##############################################################
##Exemplos 1 e 2    
##############################################################
e.civil<-c("casado","solteiro","solteiro","casado","solteiro",
           "solteiro","casado","divorciado","divorciado","solteiro",
           "solteiro","solteiro","casado","solteiro","divorciado",
           "divorciado","casado","solteiro","divorciado","casado",
           "casado","solteiro","casado","solteiro","solteiro")
cont.ec<-table(e.civil)
cont.ec
contr.ec<-prop.table(cont.ec)
contr.ec
barplot(cont.ec, main = "Gráfico em barras", 
        ylab = "Frequência", col = "rosybrown")
barplot(contr.ec, main = "Gráfico em barras", 
        ylab = "Frequência relativa", col = "rosybrown2", 
        ylim = c(0,0.5))

##############################################################
##Exemplos 3 e 4    
##############################################################
e.nut<-c("N","DM","DM","N","DG","DG","DM","DM","N","N","DG","DG",
         "DM","DM","DG","DG","DM","DM","N","N","DM","DM","N","N")
e.nut<-factor(e.nut, levels = c("DG","DM","N"), ordered = TRUE)

cont.en<-table(e.nut)
cont.en
contr.en<-round(prop.table(cont.en),2)
contr.en
barplot(cont.en, main = "Gráfico em barras", 
        ylab = "Frequência", col = "orange")
legend("topleft", c("DG: desnutrição grave", "DM: desnutrição moderada",
                    "N: nutrição"), bty = "n", cex = 0.8)
barplot(contr.en, main = "Gráfico em barras", 
        ylab = "Frequência relativa", col = "yellow", 
        ylim = c(0,0.5))
legend("topleft", c("DG: desnutrição grave", "DM: desnutrição moderada",
                    "N: nutrição"), bty = "n", cex = 0.8)

##############################################################
##Exemplos 5 e 6    
##############################################################
#data()
head(PlantGrowth)
str(PlantGrowth)

attach(PlantGrowth)
which(is.na(group))
cont.gr<-table(group)
cont.gr
contr.gr<-round(prop.table(cont.gr),2)
contr.gr
barplot(cont.gr, main = "Gráfico em barras", 
        ylab = "Frequência", col = "palegreen")

barplot(cont.gr, main = "Gráfico em barras", 
       ylab = "Frequência", col = "palegreen", 
       names.arg = c("Tratamento 1", "Tratamento 2", "Tratamento 3"))

barplot(contr.gr, main = "Gráfico em barras", 
        ylab = "Frequência relativa", col = "palegreen4", 
        ylim = c(0,0.5))
detach(PlantGrowth)

##############################################################
##Exemplos 7 e 8    
##############################################################
library(MASS)
str(UScereal)
tail(UScereal)

attach(UScereal)
which(is.na(mfr))
cont.mfr<-table(mfr)
cont.mfr
addmargins(cont.mfr)
contr.mfr<-round(prop.table(cont.mfr),2)
contr.mfr
addmargins(contr.mfr)

barplot(cont.mfr, main = "Gráfico em barras", 
        ylab = "Frequência", col = "royalblue4",
        ylim = c(0,25))
legend("topright", c("G: General Mills", "K: Kellogs","N: Nabisco",
                     "P: Post", "Q: Quaker Oats", "R: Ralston Purina"), 
       bty = "n", cex = 0.8)
barplot(contr.mfr, main = "Gráfico em barras", 
        ylab = "Frequência relativa", col = "royalblue1", 
        ylim = c(0,0.9))
legend("topright", c("G: General Mills", "K: Kellogs","N: Nabisco",
                     "P: Post", "Q: Quaker Oats", "R: Ralston Purina"), 
       bty = "n", cex = 0.8)
detach(UScereal)

##############################################################
##Exercício 1 
##################################################################
p.sat <- c("B","R","R","R","P","B","B","R","B",
          "B","E","B","B","E","B","R","R","R","R",
          "R","P","R","B","E","P","B","E","E","E",
          "B","E","R","B","B","B","B","E","E","E","R")

p.sat<-factor(p.sat, levels = c("P","R","B","E"), ordered = TRUE)

cont.sat<-table(p.sat)
cont.sat
contr.sat<-round(prop.table(cont.sat),2)
contr.sat
barplot(cont.sat, main = "Gráfico em barras", 
        ylab = "Frequência", col = "orange")
legend("topleft", c("Péssimo (P)", "Regular (R)", "Bom (B)",
                    "Excelente (E)"), bty = "n", cex = 0.8)
barplot(contr.sat, main = "Gráfico em barras", 
        ylab = "Frequência relativa", col = "yellow", 
        ylim = c(0,0.5))
legend("topleft", c("Péssimo (P)", "Regular (R)", "Bom (B)",
                    "Excelente (E)"), bty = "n", cex = 0.8)


##############################################################
##Exercico 3
##############################################################
library(MASS)
str(Cars93)
tail(Cars93)

attach(Cars93)
which(is.na(AirBags))
cont.AirBags<-table(AirBags)
cont.AirBags
addmargins(cont.AirBags)
contr.AirBags<-round(prop.table(cont.AirBags),2)
contr.AirBags
addmargins(contr.AirBags)

barplot(cont.AirBags, main = "Gráfico em barras", 
        ylab = "Frequência", col = "royalblue4",
        ylim = c(0,55))
legend("topright", c("none (Nenhum)", "driver only (Apenas
motorista)","driver & passenger (Motorista e passageiro)"), 
       bty = "n", cex = 0.8)
barplot(contr.AirBags, main = "Gráfico em barras", 
        ylab = "Frequência relativa", col = "royalblue1", 
        ylim = c(0,0.6))
legend("topright", c("none (Nenhum)", "driver only (Apenas
motorista)","driver & passenger (Motorista e passageiro)"), 
       bty = "n", cex = 0.8)
detach(Cars93)