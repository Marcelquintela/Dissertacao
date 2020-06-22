## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: Indicadores  CENSO 2000                                          #
## *************************************************************************##
 
# -------------- VARIÁVEIS TOTAIS PARA AJUSTE DO PLANO AMOSTRAL---------------
(dados$N_Dom.UF <- sum(dados$P001)) #Total de Domicílio por UF

#Total de Domicílios por Município
N_Dom.Mun <- tapply(dados$P001, dados$V0103, sum)
dados$N_Dom.Mun <- N_Dom.Mun[dados$V0103]
attributes(dados$N_Dom.Mun) <- NULL
rm(N_Dom.Mun)

#Total de Domicílios por estrato usando a Área de Ponderação  
N_Dom.AP <- tapply(dados$P001, dados$AREAP, sum)
dados$N_Dom.AP <- N_Dom.AP[dados$AREAP]
attributes(dados$N_Dom.AP) <- NULL
rm(N_Dom.AP)

# ---------- Indicadores da  Dimensão 1 - Trabalho e Rendimento ------------
# ***** D11 - Renda percapita domiciliar *****
dados <- transform(dados, 
                   cont = 1, 
                   D11 = ifelse((V7616/V7100)<=70.72,1,0))

# ***** D12 - Emprego Informal *****
dados <- transform(dados,V0447.1 = as.numeric(V0447)-1,
                         V0450.1 = as.numeric(V0450)-1)

dados <- transform(dados,Aux1 = ifelse(V4752<15," ",
                                  ifelse(V0447.1==0," ",
                                   ifelse((V0447.1==1|V0447.1==3),0,1))))

dados <- transform(dados,Aux2 = ifelse(V4752<15," ", 
                                  ifelse(V0450.1==0," ",
                                   ifelse(V0450.1==2 ,1,0))))

dados <- transform(dados, D12 = ifelse(Aux1==" " & Aux2==" ",NA, 
                                 ifelse(Aux1==" " & Aux2==  1,1,
                                  ifelse(Aux1==  1 & Aux2==" ",1,
                                   ifelse(Aux1==  1 & Aux2==  1,1,0)))))
dados <- dados[,-c(48:51)]

# ***** D13 - Auxílio Social *****
dados <- transform(dados,D13 = ifelse(V4603>0,1,0))
                                
# ***** D14 - Desocupação *****
dados <- transform(dados,D14 = ifelse(V4752>=15,
                              ifelse((V0439==2 & V0440==2 & 
                                      V0441==2 & V0442==2 & 
                                      V0443==2 & V0455==1),1,0),NA))

# ---- Indicadores da Dimensão 2 - Bens de Consumo e Acesso à Informação------
dados <- transform(dados, D21 = ifelse(V0215==2,1,0), # Geladeira
                          D22 = ifelse(V0217==2,1,0), # Maquina de Lavar Roupas
                          D23 = ifelse(V0221==0,1,0), # TV
                          D24 = ifelse(V0220==2,1,0), # Computador
                          D25 = ifelse(V0219==2,1,0)) # Acesso a telefone

# ----- Indicadores da Dimensão 3 - Condições de Saneamento e Habitação -----
dados <- transform(dados,V0211.1 = as.numeric(V0211)-1,
                         V0212.1 = as.numeric(V0212)-1)
                         
dados <- transform(dados, D31 = ifelse(V0207  == 3,1,0),  # Abastecimento de Água
                          D32 = ifelse(V0213  == 2,1,0),  # Existência de Energia Elétrica
                          D33 = ifelse(V0209  == 0,1,0),  # Banheiro de Uso Exclusivo
                          D34 = ifelse(V0211.1 > 2,1,0),  # Rede de Esgoto
                          D35 = ifelse(V0212.1 > 2,1,0),  # Destino do Lixo
                          D36 = ifelse(V7204   > 2,1,0))  # Déficit Habitacional

dados <- dados[,-c(56:57)]

# ----------- Indicadores da Dimensão 4 - Vulnerabilidade  ------------------
# ***** D41 - Deficiência *****
dados <- transform(dados,D41 = ifelse((as.numeric(V0411)<3|       # Enxergar
                                       as.numeric(V0412)<3|       # Ouvir
                                       as.numeric(V0413)<3|       # Caminhar
                                       as.numeric(V0410)<2),1,0)) # Mental/Intelectual

# ***** D42 - Presença de crianças ou Idosos no Domicílio *****
dados <- transform(dados,D42 = ifelse((V4752<15 | V4752>=65),1,0))

# * D43 - Mulheres responsáveis por Domicílio sem conjuge e com menores de 15
dados <- transform(dados, D43 = ifelse(((V0401==2 & V0402=="01")&                        
                                        (V0436==2 | V0436== 3  )),1,0))   

# ***** D44 - Jovens que não trabalha nem estuda *****
dados <- transform(dados, V0429.1 = as.numeric(V0429))
                   
dados <- transform(dados, Aux1 = ifelse((V4752<15 | V4752>29),NA,
                                  ifelse(V0429.1 > 2,1,0)))

dados <- transform(dados, Aux2 = ifelse((V4752<15 | V4752>29),NA,
                                        ifelse(V0439 == 1,0,
                                         ifelse(V0440 == 1,0,
                                          ifelse(V0441 == 1,0,
                                           ifelse(V0442 == 1,0,
                                            ifelse(V0443 == 1,0,1)))))))

#06.07.2014 - ver a condição de substituir V0442 por V0455 - Providência para conseguir trabalho

dados <- transform(dados, D44 = ifelse(Aux1==1&Aux2==1,1,0))
                                                       
dados <- dados[,-c(65:67)]

# --------------------- Indicadores da Dimensão 5 - Educação  ------------
# *** D51 - Presença de pessoas em idade ativa que não sabe ler e escrever
dados <- transform(dados, D51 = ifelse(V4752>15,NA,
                                 ifelse(V0428==2,1,0)))

# ***** D52 - Presença escolar ******
dados <- transform(dados, D52 = ifelse(V4752>=5 & V4752<=15,
                                 ifelse(V0429==3|V0429==4,1,0),NA))

# ***** D53 - Escolarização *****
dados <- transform(dados, D53 = ifelse(V4752<15,NA,
                                 ifelse(V0428==2,NA,
                                 ifelse(V0429==4|V4300<=6,1,0))))
#

# ***** D54 - Defasagem escolar *****
dados <- transform(dados,V0429.1=as.numeric(V0429),
                         V0430.1=as.numeric(V0430)-1,
                         V0431.1=as.numeric(V0431)-1)

dados <- transform(dados, Aux1 = ifelse((V4752 < 7 | V4752>18),NA,
                                  ifelse(V0429.1 > 2,NA,
                                   ifelse((V0430.1<5)            |
                                          (V4752>=15&V0430.1==7) |
                                          (V4752==18&V0430.1==9),1,0))))

dados <- transform(dados, Aux2 = ifelse((V4752 < 7 | V4752>18),NA,
                                  ifelse(V0429.1 > 2,NA,
                                   ifelse((V0430.1==5|V0430.1==6),
                                    ifelse((V4752 == 7 & V0431.1 < 1),1,
                                     ifelse((V4752 == 8 & V0431.1 < 2),1,
                                      ifelse((V4752 == 9 & V0431.1 < 3),1,
                                       ifelse((V4752 ==10 & V0431.1 < 4),1,
                                        ifelse((V4752 ==11 & V0431.1 < 5),1,
                                         ifelse((V4752 ==12 & V0431.1 < 6),1,
                                          ifelse((V4752 ==13 & V0431.1 < 7),1,
                                           ifelse((V4752 ==14 & V0431.1 < 8),1,
                                            ifelse((V4752 > 14 & V0431.1 < 9),1,
                                              0))))))))),0))))

dados <- transform(dados, Aux3 = ifelse((V4752 < 7 | V4752>18),NA,
                                  ifelse(V0429.1 > 2,NA,
                                   ifelse((V0430.1==8|V0430.1==9),
                                    ifelse((V4752 == 15 & V0431.1 < 1),1,
                                     ifelse((V4752 == 16 & V0431.1< 2),1,
                                      ifelse((V4752 == 17 & V0431.1 < 3),1,
                                       ifelse( V4752 == 18,1,
                                        0)))),0))))


dados <- transform(dados, D54 = ifelse((Aux1==1|Aux2==1|Aux3==1),1,0))

dados <- dados[,-c(69:74)]

# --------------- Apêndice  Linha de pobreza ONU--------------------
# D11.1 relativos a  linha do 2U$PPC/dia equivale a R$2,36/dia
# conforme http://unstats.un.org/unsd/mdg/Data.aspx
dados <- transform(dados, D11a = ifelse((V7616/V7100)<=70.8,1,0))

# --------------------------- AGREGAÇÃO  --------------------------
dados <- transform(dados, Aux1 = ifelse(V4752<15,1,0))
dados.Dom <- subset(dados,V0400=='01')

agreg <- c("D12","D13","D14","D41","D42","D44","D51","D52","D53","D54","Aux1") # pessoas
pos <-rep(0,length(agreg))
for(i in 1:length(agreg)){pos[i] <- which(names(dados.Dom)==agreg[i])}

a <- data.frame(rowsum(dados[,pos], dados$V0300, na.rm=T))
for(i in 1:length(agreg)){a[,agreg[i]] <- ifelse(a[,agreg[i]]>0,1,0)}
names(a) <- paste0(agreg,".1")
a <- cbind(a,V0300=rownames(a))

dados.Dom <- merge(dados.Dom, a, by="V0300")

dados.Dom <- transform(dados.Dom, D43.1 = ifelse(D43==1 & Aux1.1==1,1,0))

dados.Dom <- subset(dados.Dom,V0201==1)
dados.Dom <-dados.Dom[,c(1:47,72:74,51:61,75:76,83,77:81,70)]        

agreg <- agreg[-11];pos<-pos[-11]
names(dados.Dom)[pos] <- agreg
names(dados.Dom)[64] <- "D43"

rm(agreg,i,pos,a)
