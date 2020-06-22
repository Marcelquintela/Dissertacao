## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: Indicadores   Censo 2010                                         #
## *************************************************************************##

# -------------- VARIÁVEIS TOTAIS PARA AJUSTE DO PLANO AMOSTRAL--------------
(dados$N_Dom.UF <- sum(dados$V0010)) #Total de Domicílio por UF

#Total de Domicílios por Município 
N_Dom.Mun <- tapply(dados$V0010, dados$V0002, sum)
dados$N_Dom.Mun <- N_Dom.Mun[dados$V0002]
attributes(dados$N_Dom.Mun) <- NULL
rm(N_Dom.Mun)

#Total de Domicílios por estrato usando a Área de Ponderação 
N_Dom.AP <- tapply(dados$V0010, dados$V0011, sum)
dados$N_Dom.AP <- N_Dom.AP[dados$V0011]
attributes(dados$N_Dom.AP) <- NULL
rm(N_Dom.AP)

# ---------- Indicadores da  Dimensão 1 - Trabalho e Rendimento ------------
# ***** D11 - Renda percapita domiciliar *****
dados <- transform(dados, cont = 1, 
                          D11 = ifelse(V6531<=140,1,0))
                        
# ***** D12 - Emprego Informal *****
dados <- transform(dados,V6930.1 = as.numeric(V6930)-1,
                         V0650.1 = as.numeric(V0650)-1)

dados <- transform(dados,Aux1 = ifelse(V6036<15," ",
                                  ifelse(V6930.1==0," ",
                                   ifelse(V6930.1>2 ,1,0))))

dados <- transform(dados,Aux2 = ifelse(V6036<15," ",
                                  ifelse(V0650.1==0," ",
                                   ifelse(V0650.1==3 ,1,0))))

dados <- transform(dados, D12 = ifelse(Aux1==" " & Aux2==" ",NA, 
                                 ifelse(Aux1==" " & Aux2==  1,1,
                                  ifelse(Aux1==  1 & Aux2==" ",1,
                                   ifelse(Aux1==  1 & Aux2==  1,1,0)))))
dados <- dados[,-c(50:53)]

# ***** D13 - Auxílio Social *****
dados <- transform(dados,D13 = ifelse((V0657==9|V0657==" "|V0658==9|V0658==" "),NA,
                                ifelse((V0657==1|V0658==1),1,0)))

# ***** D14 - Desocupação *****
dados <- transform(dados,D14 = ifelse(V6910==" ",NA,
                                ifelse((V6036>=15 & V6910==2),1,0)))
#mo

# ---- Indicadores da Dimensão 2 - Bens de Consumo e Acesso à Informação------
dados <- transform(dados, D21 = ifelse(V0216==2,1,0), # Geladeira
                          D22 = ifelse(V0215==2,1,0), # Maquina de Lavar Roupas
                          D23 = ifelse(V0214==2,1,0), # TV
                          D24 = ifelse(V0219==2,1,0), # Computador 
                          D25 = ifelse(V0217==2&V0218==2,1,0))  # Telefone

# ----- Indicadores da Dimensão 3 - Condições de Saneamento e Habitação -----
dados <- transform(dados,V0207.1 = as.numeric(V0207)-1,
                         V0208.1 = as.numeric(V0208)-1,
                         V0210.1 = as.numeric(V0210)-1)

dados <- transform(dados, D31 = ifelse(V0208.1 > 2,1,0), # Abastecimento de Água
                          D32 = ifelse(V0211  == 3,1,0), # Existência de Energia Elétrica
                          D33 = ifelse(V0205  == 0,1,0), # Banheiro de Uso Exclusivo
                          D34 = ifelse(V0207.1 > 2,1,0), # Rede de Esgoto
                          D35 = ifelse(V0210.1 > 2,1,0), # Destino do Lixo
                          D36 = ifelse(V6204   > 2,1,0)) # Déficit Habitacional

dados <- dados[,-c(58:60)]

# ----------- Indicadores da Dimensão 4 - Vulnerabilidade  ------------------
# ***** D41 - Deficiência *****
dados <- transform(dados,D41 = ifelse((as.numeric(V0614)<3|       # Enxergar
                                       as.numeric(V0615)<3|       # Ouvir
                                       as.numeric(V0616)<3|       # Caminhar
                                       as.numeric(V0617)<2),1,0)) # Mental/Intelectual

# ***** D42 - Presença de crianças ou Idosos no Domicílio *****
dados <- transform(dados,D42 = ifelse((V6036<15 | V6036>=65),1,0))

# * D43 - Mulheres responsáveis por Domicílio sem conjuge e com menores de 15
dados <- transform(dados,D43 = ifelse(((dados$V0601==2 & dados$V0504=="01")&  
                                       (dados$V0637==2 | dados$V0637== 3  )),1,0))

#V0504 é o mesmo numericamente que V0502, porem conceitualmente não sao equivalentes
#para este calculo o uso de uma ou de outra não altera o resultado

# ***** D44 - Jovens que não trabalha nem estuda *****
dados <- transform(dados, V0628.1 = as.numeric(V0628))
                   
dados <- transform(dados, Aux1 = ifelse((V6036<15 | V6036>29),NA,
                                  ifelse(V0628.1 > 2,1,0)))

dados <- transform(dados, Aux2 = ifelse((V6036<15 | V6036>29),NA, 
                                  ifelse(V0641 == 1,0,1)))

dados <- transform(dados, Aux3 = ifelse((V6036<15 | V6036>29),NA,
                                  ifelse(V0642 == 1,0,
                                   ifelse(V0643 == 1,0,
                                    ifelse(V0644 == 1,0,
                                     ifelse(V0654 == 1,0,1))))))

dados <- transform(dados, D44 = ifelse(Aux1==1&Aux2==1&Aux3==1,1,0))

dados <- dados[,-c(67:70)]

# --------------------- Indicadores da Dimensão 5 - Educação  ------------
# *** D51 - Presença de pessoas em idade ativa que não sabe ler e escrever
dados <- transform(dados, D51 = ifelse(V6036>15,NA,
                                 ifelse(V0627==2,1,0)))

# ***** D52 - Presença escolar *****
dados <- transform(dados, D52 = ifelse(V6036>=5 & V6036<=15,
                                 ifelse(V0628==3|V0628==4,1,0),NA))

# ***** D53 - Escolarização *****
dados <- transform(dados, D53 = ifelse(V6036<15,NA,
                                 ifelse(V0627==2,NA, # tira as que não se declaram analfabetas
                                 ifelse(V0628==4|
                                        (V0633=="01"|V0633=="02"|
                                         V0633=="05"|V0633=="06"),1,0))))

# ***** D54 - Defasagem escolar *****
dados <- transform(dados,V0630.1=as.numeric(V0630)-1,
                         V0631.1=as.numeric(V0631)-1)

dados <- transform(dados, Aux1 = ifelse((V0630.1 == 0)," ",
                                  ifelse((V6036 < 6 | V6036>18)," ",
                                   ifelse((V6036 == 6 & V0630.1 < 1),1,
                                    ifelse((V6036 == 7 & V0630.1 < 2),1,
                                     ifelse((V6036 == 8 & V0630.1 < 3),1,
                                      ifelse((V6036 == 9 & V0630.1 < 4),1,
                                       ifelse((V6036 ==10 & V0630.1 < 5),1,
                                        ifelse((V6036 ==11 & V0630.1 < 6),1,
                                         ifelse((V6036 ==12 & V0630.1 < 7),1,
                                           ifelse((V6036 ==13 & V0630.1 < 8),1,
                                            ifelse((V6036 ==14 & V0630.1 < 9),1,
                                             ifelse((V6036 > 14 & V0630.1 <=9),1,
                                                   0)))))))))))))
dados <- transform(dados, Aux2 = ifelse((V0631.1 == 0)," ",
                                  ifelse((V6036 < 6 | V6036>18)," ",
                                   ifelse((V6036 == 15 & V0631.1 < 1),1,
                                     ifelse((V6036 == 16 & V0631.1 < 2),1,
                                      ifelse((V6036 == 17 & V0631.1 < 3),1,
                                       ifelse((V6036 == 18 & V0631.1 < 4),1,
                                              0)))))))

dados <- transform(dados, D54 = ifelse(Aux1==" " & Aux2==" ",NA,
                                 ifelse(Aux1==1|Aux2==1,1,0)))
dados <- dados[,-c(71:74)]

# --------------- Apêndice  Linha de pobreza ONU--------------------
# D11.1 relativos a  linha do 2U$PPC/dia equivale a R$3,54/dia
# conforme http://unstats.un.org/unsd/mdg/Data.aspx
dados <- transform(dados,D11a = ifelse(V6531<=106.2,1,0))

# --------------------------- AGREGAÇÃO  -------------------------
dados <- transform(dados, Aux1 = ifelse(V6036<15,1,0))
dados.Dom <- subset(dados,V0504=='01')

agreg <- c("D12","D13","D14","D41","D42","D44","D51","D52","D53","D54","Aux1")
pos <-rep(0,length(agreg))
for(i in 1:length(agreg)){pos[i] <- which(names(dados.Dom)==agreg[i])}

a <- data.frame(rowsum(dados[,pos], dados$V0300, na.rm=T))
for(i in 1:length(agreg)){a[,agreg[i]] <- ifelse(a[,agreg[i]]>0,1,0)}
names(a) <- paste0(agreg,".1")
a <- cbind(a,V0300=rownames(a))

dados.Dom <- merge(dados.Dom, a, by="V0300")

dados.Dom <- transform(dados.Dom,D43.1 = ifelse(D43==1 & Aux1.1==1,1,0))

dados.Dom <- subset(dados.Dom,V4001=='01')                  
dados.Dom <-(dados.Dom[,c(1:49,74:76,53:63,77:78,85,79:83,72)])

agreg <- agreg[-11];pos<-pos[-11]
names(dados.Dom)[pos] <- agreg                     
names(dados.Dom)[66] <- "D43"

rm(agreg,i,pos,a,dados)
