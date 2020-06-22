## *************************************************************************##
# POBREZA NO NORDESTE BRASILEIRO                                             #
# Aluno MARCEL DANTAS DE QUINTELA       Mat. 2012303131-27                   #
# Programa: CATERPILLAR                                                      #
## *************************************************************************##

# 21.07.2014
# feito manualmente para cada estado e ano... implementar automatização 
# salvo via exportaçao pdf na pasta indicada


setwd("D:/Mestrado-ENCE.2012/Dropbox/00.Dissertacao")

library(survey)
library(ggplot2)

UF <-c("Maranhão","Piauí","Ceará","Rio Grande do Norte",
       "Paraíba","Pernambuco","Alagoas","Sergipe","Bahia")

# require("xlsx")
# Mun <- read.xlsx("Municipios.xlsx", 1, encoding="UTF-8")
# save(Mun,file="07.Programa.R\\Saidas\\Municipios.rda")

# arquivo com ajuste dos nomes dos municípios com Abreviações
load("07.Programa.R\\Saidas\\Municipios.rda")
load("07.Programa.R\\PlanoAmostral\\2000-21.Rdata")

d <- data.frame(MPI,confint(MPI))
names(d)<-c("cod_mun","coef","se","inf","sup")
d <- merge(d, Mun, by="cod_mun")
d <- d[order(d$coef),]
y <-c(seq(1,nrow(d)))
y1 <-c(seq(1,nrow(d),by=2))# para saltar o nome dos municipios
d$Abrev2<-as.character(d$Abrev)
d$Abrev2[c(y[-y1])]<-c(" ")

plot.title = paste("Gráfico F.1 - Índice Multidimensional de Pobreza -",UF[1],"- 2000",sep=" ")
plot.subtitle = "Estimativas pontuais e intervalares "

p  <- ggplot(d, aes(y=coef, x=levels(cod_mun))) +
  scale_y_continuous(breaks=c(seq(0,1,by=.025)))+
  scale_x_discrete(labels=d$Abrev2)+
  ggtitle(bquote(atop(.(plot.title), 
                      atop(italic(.(plot.subtitle)(1-alpha) == 0.95, "")))))+
  geom_errorbar(aes(ymax=d$sup , ymin=d$inf), width=0.2) +
  geom_point(size=2,color="blue") +
  xlab("Municípios") + ylab("IMP")

p + theme_bw() +
  theme(
    plot.background = element_blank()
    #,panel.grid.major = element_blank()
    ,axis.text.x=element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=8))

