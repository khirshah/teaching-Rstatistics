setwd("/media/SAMSUNG/MY/works/teaching/biostatisztika/4")

########################## hf ########################################

kerd<-read.csv("kerd98p.csv")
str(kerd)
summary(kerd)
class(suly)
attach(kerd)
nev[vizsga==5&gyakjegy==5]
mean(magassag)
mean(suly[enni==5])
mean(vizsga[nehez==1],na.rm=TRUE)
mean(vizsga[nehez==2],na.rm=TRUE)

names<-rbind("magassag","suly","sulyelso","idealsuly","idealmag")
names
means<-rbind(mean(magassag),mean(suly),mean(sulyelso,na.rm=TRUE),mean(idealsly),mean(idealmag))
means
sds<-rbind(sd(magassag),sd(suly),sd(sulyelso,na.rm=TRUE),sd(idealsly),sd(idealmag))
sds
mytable<-as.data.frame(cbind(NAME=names, MEAN=means,SD=sds))
write.csv(mytable,"myfile.csv")

t.test(magassag,mu=168,alternative="g")

boxplot(magassag, idealmag)
t.test(magassag,idealmag,paired=TRUE)
boxplot(suly,idealsly)
t.test(suly,idealsly,paired=TRUE)

################################ nonparametric tests ###################################


############# one sample #################

T<-read.csv("leveltetu.csv")
summary(T)
attach(T)
hist(x,col="purple",main="histogram of a")
plot(density(x),main="Density estimate of a")
qqnorm(x); qqline(x)
mean(x)
?wilcox.test
wilcox.test(x,mu=19)
wilcox.test(x,alternative="g",mu=19)
wilcox.test(x,alternative="less",mu=19)
wilcox.test(x,alternative="g",mu=15)

############### paired  ###############################

intake<-read.csv("intake.csv")
intake
summary(intake)
attach(intake)
hist(post-pre)
plot(density(post-pre), main="surusegfuggveny")
qqnorm(post-pre);qqline(post-pre)
wilcox.test(pre,post, paired=TRUE,exact=FALSE)


fogyas<-read.csv("befafter.csv")
fogyas
attach(fogyas)
hist(AFTER-BEFORE)

wilcox.test(BEFORE,AFTER,paired=TRUE)
wilcox.test(BEFORE,AFTER,paired=TRUE,alternative="g")

############################ Mann-Whitney #############################

b<-read.csv("Zootaxadatok.csv")
b
attach(b)
str(b)
summary(b)
hist(Szem_H,col="cyan4",main="Himek szeme")
hist(Szem_N,col="indianred2",main="Nostenyek szeme")
hist(Fejteto_H,col="darkolivegreen2",main="Himek fejteto")
hist(Fejteto_N,col="brown1",main="Nostenyek fejteto")
hist(Hossz_H,col="darkslategray",main="Himek hossza")
hist(Hossz_N,col="darksalmon",main="Nostenyek hossza")

var.test(Szem_H,Szem_N)
var(Szem_H,na.rm=TRUE)
var(Szem_N,na.rm=TRUE)
boxplot(Szem_H,Szem_N)
wilcox.test(Szem_H, Szem_N)
var(Fejteto_H,na.rm=TRUE)
var(Fejteto_N,na.rm=TRUE)
boxplot(Fejteto_H,Fejteto_N)
var(Hossz_H,na.rm=TRUE)
var(Hossz_N,na.rm=TRUE)
var.test(Fejteto_H,Fejteto_N)
boxplot(Fejteto_H,Fejteto_N)
wilcox.test(Fejteto_H,Fejteto_N,var.equal=FALSE)
t.test(Fejteto_H,Fejteto_N)
var.test(Hossz_H,Hossz_N)
wilcox.test(Hossz_H, Hossz_N,var.equal=FALSE)

##################### kerdoiv #############################

suly[nem==1]
boxplot(suly[nem==1],suly[nem==2],main="suly")
var.test(suly[nem==1],suly[nem==2])
wilcox.test(suly[nem==1],suly[nem==2],exact=FALSE)
t.test(suly[nem==1],suly[nem==2],exact=FALSE)

boxplot(sulyelso[nehez==1],sulyelso[nehez==2])
var.test(sulyelso[nehez==1],sulyelso[nehez==2])
wilcox.test(suly[nehez==1],suly[nehez==2])

boxplot(idealsly[nem==1],idealsly[nem==2])
var.test(idealsly[nem==1],idealsly[nem==2])
wilcox.test(idealsly[nem==1],idealsly[nem==2])

boxplot(magassag[nehez==1],magassag[nehez==2],main="magassag-nehez")
var.test(magassag[nehez==1],magassag[nehez==2])
wilcox.test(magassag[nehez==1],magassag[nehez==2])
