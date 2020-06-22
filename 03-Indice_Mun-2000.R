## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: Saidas                                                           #
## *************************************************************************##

library(xlsx)
library(survey)
toBibtex(citation("survey"))
toBibtex(citation())

setwd("D:\\Mestrado-ENCE.2012\\Dropbox\\00.Dissertacao")
c <- c(22:29)
prog <- '07.Programa.R\\03.1-Esti_Mun-2000.R'
for (j in 1:length(c)){
  load(paste("04.Banco.de.Dados\\Banco\\2000_Dom",c[j],".rda",sep=""))
  source(prog, echo=TRUE)
  save(AES,H,A,MPI,C.i, file=paste("07.Programa.R\\PlanoAmostral\\2000-",c[j],".Rdata",sep=""))
  write.xlsx(X, paste("07.Programa.R\\saidas\\Ind_Municipio\\Tabelas\\",c[j],"_2000.xlsx",sep=""))
  rm(dados.Dom,AES,X,MPI,H,A,C.i)
}
rm(c,prog,j)