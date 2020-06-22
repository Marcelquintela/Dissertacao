## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: ESTIMATIVAS                                                      #
## *************************************************************************## 

# ------------------- Scores de Privação  ----------------------------------
dados.Dom <- transform(dados.Dom, SD1 = ((D11+D12+D13+D14        )*1/4*1/5),
                                  SD2 = ((D21+D22+D23+D24+D25    )*1/5*1/5),
                                  SD3 = ((D31+D32+D33+D34+D35+D36)*1/6*1/5),
                                  SD4 = ((D41+D42+D43+D44        )*1/4*1/5),
                                  SD5 = ((D51+D52+D53+D54        )*1/4*1/5))
dados.Dom <- transform(dados.Dom,   C = SD1+SD2+SD3+SD4+SD5)

# --------------- Indicadoras de Domicilio em Pobreza ------------------------
dados.Dom <- transform(dados.Dom, NPob = ifelse( C<0.20,1,0),
                                  Vul  = ifelse((C>=0.20 & C<0.33),1,0),
                                  Pob  = ifelse( C>=0.33,1,0),
                                  PobG = ifelse( C>=0.50,1,0))


# como o banco esta agregado por domicí�lio a expansão para pessoas é feita por:
# V7100-Nº pessoas residentes no domicí�lio
dados.Dom <- transform(dados.Dom, Pes_NPob = NPob*V7100,
                                  Pes_Vul  = Vul *V7100,
                                  Pes_Pob  = Pob *V7100,
                                  Pes_PobG = PobG*V7100,
                                  Pes_D11  = D11 *V7100,
                                  Pes_D11a = D11a*V7100)

dados.Dom <- transform(dados.Dom, SumC   = ifelse(Pob==1,  C*V7100,0),
                                  SumC.1 = ifelse(Pob==1,SD1*V7100,0),
                                  SumC.2 = ifelse(Pob==1,SD2*V7100,0),
                                  SumC.3 = ifelse(Pob==1,SD3*V7100,0),
                                  SumC.4 = ifelse(Pob==1,SD4*V7100,0),
                                  SumC.5 = ifelse(Pob==1,SD5*V7100,0))

AES <- svydesign (id = ~1,
                  strata = ~AREAP,
                  fpc = ~N_Dom.AP,
                  weights = ~P001,
                  data = dados.Dom)

#Incidencia de Não Pobres, Pobres (H),vulneráveis e em Pobreza Grave
# Representa a proporção da população que são multidimensionalmente pobre: H=q/n
# H <- svyby(formula  = ~Pob+NPob+Vul+PobG+D11+D11a,
#            by       = ~V0103,
#            design   = AES, 
#            na.rm    = TRUE, 
#            svymean)

H <- svyby(formula    = ~Pes_Pob+Pes_NPob+Pes_Vul+Pes_PobG+Pes_D11+Pes_D11a,
           by          = ~V0103,
           denominator = ~V7100,
           design      = AES, 
           svyratio)
rownames(H)<-NULL

#Intencidade de Pobreza (A)
A <- svyby(formula     = ~SumC,
           by          = ~V0103,
           denominator = ~Pes_Pob, # denominador número de pobres
           design      = AES, 
           svyratio)
rownames(A)<-NULL

#MPI
MPI <- svyby(formula     = ~SumC,
             by          = ~V0103,
             denominator = ~V7100,
             design      = AES, 
             svyratio)
rownames(MPI)<-NULL

#Contribuição da dimensão
C.i <- svyby(formula     = ~SumC.1+SumC.2+SumC.3+SumC.4+SumC.5,
             by          = ~V0103,
             denominator = ~SumC,
             design      = AES, 
             svyratio)
rownames(C.i)<-NULL

X <- data.frame(MPI$V0103,
                coef(MPI)  , round(cv(MPI)*100,4)      ,
                H[,3]*100  , c(round(cv(H)[2]*100,4))  ,
                H[,2]*100  , c(round(cv(H)[1]*100,4))  ,
                H[,4]*100  , c(round(cv(H)[3]*100,4))  ,
                H[,5]*100  , c(round(cv(H)[4]*100,4))  ,
                H[,6]*100  , c(round(cv(H)[5]*100,4))  ,
                H[,7]*100  , c(round(cv(H)[6]*100,4))  ,
                coef(A)*100, round(cv(A)*100,4)        ,
                C.i[,2]*100, c(round(cv(C.i)[2]*100,4)),
                C.i[,3]*100, c(round(cv(C.i)[1]*100,4)),
                C.i[,4]*100, c(round(cv(C.i)[3]*100,4)),
                C.i[,5]*100, c(round(cv(C.i)[4]*100,4)),
                C.i[,6]*100, c(round(cv(C.i)[5]*100,4)))

names(X) <- c("V0103",
              "MPI"       ,"CV.MPI",
              "NaoPobres (%)","CV.NaoPobre (%)",
              "Pobres (%)","CV.Pobre (%)",
              "Vulneraveis (%)","CV.Vulneraveis (%)",
              "Pobres Graves (%)","CV.Pobre Graves (%)",
              "Pobre LP-MDS(%)","CV.Pobre LP-MDS(%)",
              "Pobre LP-2PPC(%)","CV.Pobre LP-2ppc(%)",
              "Intensidade.Priva(%)","CV.Intensidade(%)",
              "Contrib.D_1 (%)","CV.D_1 (%)",
              "Contrib.D_2 (%)","CV.D_2 (%)",
              "Contrib.D_3 (%)","CV.D_3 (%)",
              "Contrib.D_4 (%)","CV.D_4 (%)",
              "Contrib.D_5 (%)","CV.D_5 (%)")