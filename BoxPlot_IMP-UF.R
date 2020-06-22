library(xtable)
options(digits=4)

setwd("D:\\Mestrado-ENCE.2012\\Dropbox\\00.Dissertacao\\07.Programa.R\\Saidas\\Ind_Municipio")
x<-read.csv("Razao.csv",header=TRUE, sep=";", quote="\"", dec=",")

plot(x$IMP.2000,x$IMP.2010, ylim=c(0,1),xlim=c(0,1),pch=20, xlab=expression(IMP[2000]),ylab=expression(IMP[2010]),cex.lab=1.5, cex.axis=1.5)
abline(0,1,col="red")

oind <- order(as.numeric(by(x$Razão.2010.2000, x$UF, median,na.rm=T)))    
#ord <- c("MA","PI","CE","RN","PB","PE","AL","SE","BA")

x$UF <- ordered(x$UF, levels=levels(x$UF)[oind]) 

y<-boxplot(Razão.2010.2000~UF, data=x, pch=20,col="lightblue", cex.lab=1.5, cex.axis=1.5)

y$out[y$out>0.8]
p1<-c(0,0.8216,0,0.8321,0.8678,0,0,0,0.8751)
p2<-c(rep(0,8),0.8639)
points(p1, col="firebrick1", pch=20)
points(p2, col="firebrick1", pch=20)




#Outlier
outlier <- x[1,]
outlier <- outlier[-1,]
for(i in 1:length(y$names)) {
  a <- subset(x, UF==y$names[i])[
         which(x$Razão.2010.2000[x$UF==y$names[i]] < y$stats[1,i]|
              x$Razão.2010.2000[x$UF==y$names[i]] > y$stats[5,i]),]
  outlier <- rbind(outlier,a)
}

rownames(outlier)<-NULL

tab<-xtable(outlier[order(outlier$UF,outlier$Razão.2010.2000),c(4:7)])
digits(tab)<-digits(tab)*2

print(tab,
      floating=FALSE,
      include.rownames=FALSE)



