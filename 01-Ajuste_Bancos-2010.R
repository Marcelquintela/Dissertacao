## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA     Mat. 2012303131-27                     #
# Programa: AJUSTE DOS BANCOS DE DADOS                                       #
## *************************************************************************##

# Biblioteca para leitura e escrita de dados em outros formatos
library(foreign)

setwd("D:\\Mestrado-ENCE.2012\\Dropbox\\00.Dissertacao")

c <- c(21:28,"29a","29b")
c <-29
for (i in 1:length(c)){
  dados <- read.spss(paste("04.Banco.de.Dados\\Banco\\2010-",c[i],".sav",sep="")
                     ,use.value.labels=F,to.data.frame=T)
  save(dados, file=paste("04.Banco.de.Dados\\Banco\\2010-",c[i],".rda",sep=""))
  rm(dados)
}

rm(c,i)

