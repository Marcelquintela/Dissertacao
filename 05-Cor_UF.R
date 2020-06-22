## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: Cerrelação dos Indicadores Simples Estados                       #
## *************************************************************************##
# 
setwd("D:\\Mestrado-ENCE.2012\\Dropbox\\00.Dissertacao")
library(xlsx)

c <- c(21:29)

for (j in 1:length(c)){
  load(paste("04.Banco.de.Dados\\Banco\\2000_Dom",c[j],".rda",sep=""))
  a <- cor(dados.Dom[,c(47:69)],method=c("pearson"))
  b <- cor(dados.Dom[,c(47:69)],method=c("spearman"))
  write.xlsx(round(rbind(a,c(rep(0,23)),b),4), 
             paste("07.Programa.R/saidas/Ind_Estado/Cor/Cor_",c[j],"-2000.xlsx",sep=""))
  
  load(paste("04.Banco.de.Dados\\Banco\\2010_Dom",c[j],".rda",sep=""))
  c <- cor(dados.Dom[,c(49:71)],method=c("pearson"))
  d <- cor(dados.Dom[,c(49:71)],method=c("spearman"))
  write.xlsx(round(rbind(a,c(rep(0,23)),b),4), 
             paste("07.Programa.R/saidas/Ind_Estado/Cor/Cor_",c[j],"-2010.xlsx",sep=""))
}
