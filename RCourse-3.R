setwd("/media/store/works/teaching/biostatisztika/3")

################################### hf ##################################
d<-read.csv("csaszar.csv")
d
attach(d)

pO2[5:10]
HCO3[c(8,10,12)]
SaO2[SaO2>3 & SaO2<18]
pH[elektiv.mutet=="nem"]
BE[pH==7.21|pH==7.28]

hist(pH,col="darksalmon",main="hisztogram a ph-rol",xlab="pH",ylab="gyakorisag")
plot(ecdf(pH))
plot(density(pH), col="red",main="pH surusegfuggveny",xlab="pH",ylab="pdf")
qqnorm(pH); qqline(pH)
boxplot(pH)
a<-rbind(mean(pH),sd(pH),min(pH),max(pH),median(pH))
a
b<-rbind("mean","SD","minimum","maximum","median")
b
c<-as.data.frame(cbind(b,a))
c

############# one sample t-test #################

T<-read.csv("leveltetu.csv")
summary(T)
attach(T)
hist(x,col="purple",main="histogram of a")
plot(density(x),main="Density estimate of a")
qqnorm(x); qqline(x)
mean(x)
?t.test
t.test(T,mu=19)
t.test(T,alternative="g",mu=19)
t.test(T,alternative="less",mu=19)
t.test(T,alternative="g",mu=15)

############### paired t-test ###############################

intake<-read.csv("intake.csv")
intake
summary(intake)
attach(intake)
hist(post-pre)
plot(density(post-pre), main="surusegfuggveny")
qqnorm(post-pre);qqline(post-pre)
t.test(pre,post, paired=TRUE, conf.level=0.99)


fogyas<-read.csv("befafter.csv")
fogyas
attach(fogyas)
hist(AFTER-BEFORE)
t.test(BEFORE,AFTER,paired=TRUE)
t.test(BEFORE,AFTER,paired=TRUE,alternative="g")

############################ independent t-test #############################

b<-read.csv("Zootaxadatok.csv")
b
attach(b)

hist(Szem_H,col="cyan4",main="Himek szeme")
hist(Szem_N,col="indianred2",main="Nostenyek szeme")
hist(Fejteto_H,col="darkolivegreen2",main="Himek fejteto")
hist(Fejteto_N,col="brown1",main="Nostenyek fejteto")
hist(Hossz_H,col="darkslategray",main="Himek hossza")
hist(Hossz_N,col="darksalmon",main="Nostenyek hossza")

var.test(Szem_H,Szem_N)
t.test(Szem_H, Szem_N)
var.test(Fejteto_H,Fejteto_N)
var(Fejteto_H,rm.na=TRUE)
boxplot(Fejteto_H,Fejteto_N)
t.test(Fejteto_H,Fejteto_N,var.equal=FALSE)
var.test(Hossz_H,Hossz_N)
t.test(Hossz_H, Hossz_N,var.equal=FALSE)
