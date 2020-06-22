## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: Saidas                                                           #
## *************************************************************************##

library(xlsx)
library(survey)

setwd("D:\\Mestrado-ENCE.2012\\Dropbox\\00.Dissertacao")

c <- c(21:29)
prog <- '07.Programa.R\\03.1-Esti_Mun-2010.R'
for (j in 1:length(c)){
  load(paste("04.Banco.de.Dados\\Banco\\2010_Dom",c[j],".rda",sep=""))
  source(prog, echo=TRUE)
  save(AES,H,A,MPI,C.i, file=paste("07.Programa.R\\PlanoAmostral\\2010-",c[j],".Rdata",sep=""))
  write.xlsx(X, paste("07.Programa.R\\saidas\\Ind_Municipio\\Tabelas\\",c[j],"_2010.xlsx",sep=""))
  rm(dados.Dom,AES,X,MPI,H,A,C.i)
}
rm(c,prog,j)