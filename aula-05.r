##############################################################
##BDA - AULA 5
##############################################################
##Exemplo 1    
##############################################################
notas<-c(174,172,169,176,169,170,175)
Md.notas<-median(notas)
cat("Mediana das notas = ",Md.notas, "\n")

##############################################################
##Exemplo 2    
##############################################################
xi<-c(0,1,2,3,4,5)
fi<-c(5,6,7,12,2,3)
filhos<-rep(xi,fi)

Md.filhos<-median(filhos)
cat("Mediana  = ",Md.filhos, "\n")

##############################################################
##Exemplo 3    
##############################################################
install.packages("ebda.tar.gz", repos = NULL, type = "source")
library(ebda)

k<-7
h<-500
L0<-1000
fi<-c(8,10,11,16,13,5,2)

tab<-tab.df(k,h,L0,fi)

Md.sal<-m.s(tab,1,2)
cat("Mediana = ", round(Md.sal,2), "\n")


##############################################################
##Exemplo 4    
##############################################################
set.seed(123)
n <- 200
gênero <- sample(c("F", "M"), size = n, replace = TRUE)

instrução <- sample(c("Ensino fundamental", "Ensino médio", "Ensino superior"), 
                    size = n, replace = TRUE) 
instrução<-factor(instrução, 
                  levels = c("Ensino fundamental", "Ensino médio", "Ensino superior"), 
                  ordered = TRUE)
idade <- sample(18:70, size = n, replace = TRUE)

aposentado <- ifelse(idade >= 60,
                     sample(c("S", "N"), size = n, replace = TRUE, prob = c(0.6, 0.4)),
                     "N")

dados <- data.frame(
  gênero = gênero,
  instrução = instrução,
  idade = idade,
  aposentado = aposentado
)

Md.idade<-median(dados$idade)
cat("Mediana da idade = ", Md.idade, "\n")

Md.ins <- levels(dados$instrução)[median(as.numeric(dados$instrução))]
cat("Mediana do grau de instrução = ", Md.ins, "\n")

filtro<-subset(dados, gênero == "F" & aposentado == "S", select = idade)
cat("Mediana da idade das mulheres aposentadas  = ", median(filtro$idade), "\n")

tab <- table(dados$instrução, dados$gênero)
#S<-addmargins(tab)

barplot(tab, main = "Grau de instrução por gênero",
               xlab = "Gênero",
               ylab = "Frequência", ylim = c(0,60),
               col = c("skyblue1", "blue", "lightblue"),
               legend.text = rownames(tab),
               args.legend = list(x = "topright",
                                  bty = "n",
                                  cex = 0.8),
               beside = TRUE)  


idades = c(25,30,22,45,28,32)
sexo = c("F","M","F","M","F","M")

tapply(idades,sexo, mean)
