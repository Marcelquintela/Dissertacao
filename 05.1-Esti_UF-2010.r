## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: ESTIMATIVAS                                                      #
## *************************************************************************##
# 

library(survey)
library(xlsx)

setwd("D:\\Mestrado-ENCE.2012\\Dropbox\\00.Dissertacao")
load("SE2010.Rdata")
save(H,MPI,C.i,A, file="SE2010.Rdata")

UF <-c("Maranhão","Piauí","Ceará","Rio Grande do Norte",
       "Paraíba","Pernambuco","Alagoas","Sergipe","Bahia")
Y <- data.frame(matrix(NA,1,27,byrow=T))
names(Y) <- c("UF",
              "MPI"       ,"CV.MPI",
              "Pobres (%)","CV.Pobre (%)",
              "NaoPobres (%)","CV.NaoPobre (%)",
              "Vulneraveis (%)","CV.Vulneraveis (%)",
              "Pobres Graves (%)","CV.Pobre Graves (%)",
              "Intensidade.Priva(%)","CV.Intensidade(%)",
              "Contrib.D_1 (%)","CV.D_1 (%)",
              "Contrib.D_2 (%)","CV.D_2 (%)",
              "Contrib.D_3 (%)","CV.D_3 (%)",
              "Contrib.D_4 (%)","CV.D_4 (%)",
              "Contrib.D_5 (%)","CV.D_5 (%)",
              "Pobre LP-2PPC(%)","CV.Pobre LP-2ppc(%)",
              "Pobre LP-MDS(%)","CV.Pobre LP-MDS(%)")
c <- c(21:29)

for (j in 1:length(c)){
  load(paste("04.Banco.de.Dados/Banco/2010_Dom",c[j],".rda",sep=""))
# ------------------- Scores de Privação  ----------------------------------
dados.Dom <- transform(dados.Dom, SD1 = ((D11+D12+D13+D14        )*1/4*1/5),
                                  SD2 = ((D21+D22+D23+D24+D25    )*1/5*1/5),
                                  SD3 = ((D31+D32+D33+D34+D35+D36)*1/6*1/5),
                                  SD4 = ((D41+D42+D43+D44        )*1/4*1/5),
                                  SD5 = ((D51+D52+D53+D54        )*1/4*1/5))

dados.Dom <- transform(dados.Dom, C = SD1+SD2+SD3+SD4+SD5)

dados.Dom <- transform(dados.Dom, NPob = ifelse( C<0.20,1,0),
                       Vul  = ifelse((C>=0.20 & C<0.33),1,0),
                       Pob  = ifelse( C>=0.33,1,0),
                       PobG = ifelse( C>=0.50,1,0))


# ------------------- Incidência - H  ----------------------------------
# Representa a proporção da população multidimensionalmente pobre:H=q/n
# onde q é o número de pessoas que estão multidimensionalmente pobre e 
# n a população total por dominio = município.

# como o banco esta agregado p/ dom. a expansão p/ pessoas é feita por:
# V0401-Nº pessoas residentes no domicílio

dados.Dom <- transform(dados.Dom, q = ifelse(Pob==1,V0401,0))

dados.Dom <- transform(dados.Dom, SumC   = ifelse(Pob==1,C*V0401,0),
                                  SumC.1 = ifelse(Pob==1,SD1*V0401,0),
                                  SumC.2 = ifelse(Pob==1,SD2*V0401,0),
                                  SumC.3 = ifelse(Pob==1,SD3*V0401,0),
                                  SumC.4 = ifelse(Pob==1,SD4*V0401,0),
                                  SumC.5 = ifelse(Pob==1,SD5*V0401,0))

AES <- svydesign (id = ~1,
                  strata = ~V0011,
                  fpc = ~N_Dom.AP,
                  weights = ~V0010,
                  data = dados.Dom)

#Incidencia de domicílios pobres e vulneráveis - H 
# inclui o indicador de renda domiciliar per capta para ser comparada
# Linha nacional e o da ONU (2U$PPP/dia) em 2000 estas linhas 
# se equivalem R$70,8 
# Lembrando que a linha em 2000 é a mesma de 2010 deflacionana pelo INPC
#(divide pelo total de domicílios do municipio)
H <- svymean(~Pob+NPob+Vul+PobG+D11a+D11,
             design   = AES, 
             na.rm    = TRUE)
rownames(H)<-NULL

#Intencidade (A)
A <- svyratio(numerator   = ~SumC,
              denominator = ~q, # denominador número de pobres
              design      = AES, 
              na.rm       =  TRUE)
         
rownames(A)<-NULL

#MPI
MPI <- svyratio(numerator   = ~SumC,
                denominator = ~V0401,
                design      = AES, 
                na.rm       =  TRUE)
rownames(MPI)<-NULL

#Contribuição da dimensão
C.i <- svyratio(numerator   = ~SumC.1+SumC.2+SumC.3+SumC.4+SumC.5,
                denominator = ~SumC,
                design      = AES, 
                na.rm       =  TRUE)
rownames(C.i)<-NULL
coef(C.i)

X <- data.frame(UF[j],
                coef(MPI)    , round(cv(MPI)*100,4) ,
                H[1]*100     , round(cv(H)[2]*100,4),
                H[2]*100     , round(cv(H)[1]*100,4),
                H[3]*100     , round(cv(H)[3]*100,4),
                H[4]*100     , round(cv(H)[4]*100,4),
                coef(A)*100  , round(cv(A)*100,4)   ,
                coef(C.i)[1]*100  , round(cv(C.i)[1]*100,4),
                coef(C.i)[2]*100  , round(cv(C.i)[2]*100,4),
                coef(C.i)[3]*100  , round(cv(C.i)[3]*100,4),
                coef(C.i)[4]*100  , round(cv(C.i)[4]*100,4),
                coef(C.i)[5]*100  , round(cv(C.i)[5]*100,4),
                H[5]*100     , round(cv(H)[3]*100,4),
                H[6]*100     , round(cv(H)[4]*100,4))

names(X) <- c("UF",
              "MPI"       ,"CV.MPI",
              "Pobres (%)","CV.Pobre (%)",
              "NaoPobres (%)","CV.NaoPobre (%)",
              "Vulneraveis (%)","CV.Vulneraveis (%)",
              "Pobres Graves (%)","CV.Pobre Graves (%)",
              "Intensidade.Priva(%)","CV.Intensidade(%)",
              "Contrib.D_1 (%)","CV.D_1 (%)",
              "Contrib.D_2 (%)","CV.D_2 (%)",
              "Contrib.D_3 (%)","CV.D_3 (%)",
              "Contrib.D_4 (%)","CV.D_4 (%)",
              "Contrib.D_5 (%)","CV.D_5 (%)",
              "Pobre LP-2PPC(%)","CV.Pobre LP-2ppc(%)",
              "Pobre LP-MDS(%)","CV.Pobre LP-MDS(%)")

row.names(X)<-NULL
Y <- rbind(Y,X)
}

Y <- Y[-1,]
row.names(Y)<-NULL
write.xlsx(Y, "07.Programa.R/saidas/Ind_Estado/MPI_UF2010.xlsx")
