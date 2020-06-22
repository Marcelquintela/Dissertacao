## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: ESTIMATIVAS                                                      #
## *************************************************************************##

setwd("D:\\Mestrado-ENCE.2012\\Dropbox\\00.Dissertacao")

#c <- c(21:28,"29a","29b")
c <- c(21:29)

prog <- '07.Programa.R\\02.1-Indicadores-2000.R'
for (j in 1:length(c)){
  load(paste("04.Banco.de.Dados\\Banco\\2000-",c[j],".rda",sep=""))
  source(prog, echo=TRUE)
  save(dados.Dom, file=paste("04.Banco.de.Dados\\Banco\\2000_Dom",c[j],".rda",sep=""))
  rm(dados.Dom)
}
rm(c,prog,j)
