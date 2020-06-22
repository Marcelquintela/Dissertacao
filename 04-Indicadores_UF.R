## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: INDICADORES SIMPLES                                              #
## *************************************************************************##
# 
setwd("D:/Mestrado-ENCE.2012/Dropbox/00.Dissertacao")

library(xlsx)
library(survey)
#c<-28;UF<-"Sergipe";j<-1

c <- c(21:29)
UF <-c("Maranhão","Piauí","Ceará","Rio Grande do Norte",
       "Paraíba","Pernambuco","Alagoas","Sergipe","Bahia")
Dim <-c("D11-Renda Domiciliar","D12-Emprego Informal","D13-Auxílio Social","D14-Desocupação",
        "D21-Geladeira","D22-Máquina de Lavar","D23-Televisão","D24-Microcomputador","D25-Telefone",
        "D31-Água","D32-Energia Elétrica","D33-Banheiro","D34-Esgoto","D35-Lixo","D36-Deficit Habitacional",
        "D41-Portadores de Necessidades Especiais","D42-Atenção a Idosos e Crianças",
        "D43-Mulheres sem Companheiros e com Crianças","D44-Jovens que não Trabalha nem Estuda",
        "D51-Analfabetismo","D52-Ausência Escolar","D53-Escolarização","D54-Defasagem Escolar")

for (j in 1:length(c)){
  load(paste("04.Banco.de.Dados/Banco/2000_Dom",c[j],".rda",sep=""))
  dados.Dom <- transform(dados.Dom, 
                         D12a   = D12*V7100,D13a   = D13*V7100,
                         D14a   = D14*V7100,D44a   = D44*V7100,
                         D51a   = D51*V7100,D52a   = D52*V7100,
                         D53a   = D53*V7100,D54a   = D54*V7100)

  AES <- svydesign (id = ~1,
                    strata = ~AREAP,
                    fpc = ~N_Dom.AP,
                    weights = ~P001,
                    data = dados.Dom)
    #DOMICILIO
  # refazer incluindo D41 e D42
  D  <- svymean(~D11 +
                  D21 +D22 +D23 +D24 +D25 +
                  D31 +D32 +D33 +D34 +D35 +D36 +
                  D41 +D42 +D43,
                  design      =  AES,
                  na.rm       =  TRUE)
  
  #PESSOAS
  P  <- svyratio(numerator   = ~D12a+D13a+D14a+
                                D44a+
                                D51a+D52a+D53a+D54a,
                 denominator = ~V7100,
                 design      =  AES,
                 na.rm       =  TRUE)
  
  D.1 <- data.frame(coef(D))
  x <- data.frame(c(D.1[1,1],P$ratio[1:3],D.1[c(2:15),1],P$ratio[4:8]))
  names(x) <- c("x")
  row.names(x) <-c("D11","D12","D13","D14",
                   "D21","D22","D23","D24","D25",
                   "D31","D32","D33","D34","D35","D36",
                   "D41","D42","D43","D44",
                   "D51","D52","D53","D54")
  
  x1<-data.frame(confint(D)[c(1:15),]);names(x1)<-c("2.5%","97.5%")
  x2<-data.frame(confint(P));names(x2)<-c("2.5%","97.5%")
  x1<-rbind(x1[1,],x2[c(1:3),],x1[c(2:15),],x2[c(4:8),])
  x<-cbind(x,x1)
  names(x)<-c("Indicador_2000","Inf","Sup")
  rm(dados.Dom,AES,D,P,D.1,x1,x2)
  
  #------------------------------------#
  
  load(paste("04.Banco.de.Dados\\Banco\\2010_Dom",c[j],".rda",sep=""))
  
  dados.Dom <- transform(dados.Dom, 
                         D12a   = D12*V0401,D13a   = D13*V0401,
                         D14a   = D14*V0401,D44a   = D44*V0401,
                         D51a   = D51*V0401,D52a   = D52*V0401,
                         D53a   = D53*V0401,D54a   = D54*V0401)
  
  AES <- svydesign (id = ~1,
                    strata = ~V0011,
                    fpc = ~N_Dom.AP,
                    weights = ~V0010,
                    data = dados.Dom)
  
  #DOMICILIO
  D  <- svymean(~D11 +
                  D21 +D22 +D23 +D24 +D25 +
                  D31 +D32 +D33 +D34 +D35 +D36 +
                  D41 +D42 +D43,
                design      =  AES,
                na.rm       =  TRUE)
  
  #PESSOAS
  P  <- svyratio(numerator   = ~D12a+D13a+D14a+
                                D44a+
                                D51a+D52a+D53a+D54a,
                 denominator = ~V0401,
                 design      =  AES,
                 na.rm       =  TRUE)
  
  D.1 <- data.frame(coef(D))
  y <- data.frame(c(D.1[1,1],P$ratio[1:3],D.1[c(2:15),1],P$ratio[4:8]))
  names(y) <- c("x")
  row.names(y) <-c("D11","D12","D13","D14",
                   "D21","D22","D23","D24","D25",
                   "D31","D32","D33","D34","D35","D36",
                   "D41","D42","D43","D44",
                   "D51","D52","D53","D54")
  
  x1<-data.frame(confint(D)[c(1:15),]);names(x1)<-c("2.5%","97.5%")
  x2<-data.frame(confint(P));names(x2)<-c("2.5%","97.5%")
  
  x1<-rbind(x1[1,],x2[c(1:3),],x1[c(2:15),],x2[c(4:8),])
  y<-cbind(y,x1)
  names(y)<-c("Indicador_2010","Inf","Sup")
  rm(dados.Dom,AES,D,P,D.1,x1,x2)
 
  #-----------------------------------------#
  a <-cbind(x,y);row.names(a)<-Dim
  write.xlsx(round(a,4)*100, paste("07.Programa.R/saidas/Ind_Estado/Ind-",c[j],".xlsx",sep=""))
  rm(x,y,a)
}


