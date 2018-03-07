setwd("/media/SAMSUNG/MY/works/teaching/biostatisztika/5")

########################### korrelacio ######################################

kerd<-read.csv("kerd98p.csv")
attach(kerd)


plot(suly, magassag)
abline(lm(magassag~suly), col="red", lwd=3)
cor(suly, magassag)

conf.lines<-predict(lm(magassag~suly), interval="confidence") 
conf.lines
lines(suly, conf.lines[,2],col="green")
lines(suly, conf.lines[,3], lty=2)


#### intake 
intake<-read.csv("intake.csv")
intake
summary(intake)
attach(intake)
cor(pre,post)
plot(pre,post)
abline(lm(post~pre), col="red", lwd=3)

conf.lines<-predict(lm(post~pre), interval="confidence") 
lines(pre, conf.lines[,2], lty=2)
lines(pre, conf.lines[,3], lty=2)



##### rovar

b<-read.csv("Zootaxadatok.csv")
attach(b)
str(b)
summary(b)
plot(b)

cor(Szem_N,Fejteto_N)
plot(Szem_N,Fejteto_N,pch=17, col="darkblue")
abline(lm(Fejteto_N~Szem_N), col="purple", lwd=3)

conf.lines<-predict(lm(Fejteto_N~Szem_N), interval="confidence") 
lines(Szem_N, conf.lines[,2], lty=2)
lines(Szem_N, conf.lines[,3], lty=2)

lm(Fejteto_N~Szem_N)
summary(lm(Fejteto_N~Szem_N))


###### fogyas
fogyas<-read.csv("befafter.csv")
fogyas
attach(fogyas)
plot(BEFORE,AFTER)
abline(lm(AFTER~BEFORE), col="darkgreen", lwd=3)
cor(BEFORE,AFTER)

conf.lines<-predict(lm(AFTER~BEFORE), interval="confidence") 
lines(BEFORE, conf.lines[,2], lty=2)
lines(BEFORE, conf.lines[,3], lty=2)

summary(lm(AFTER~BEFORE))

### antropometria
a<-read.csv("antropometria.csv")
str(a)
attach(a)
plot(TT1,TT2)
abline(lm(TT2~TT1), col="darkgreen", lwd=3)
cor(TT1,TT2)
conf.lines<-predict(lm(TT2~TT1), interval="confidence") 
lines(TT1, conf.lines[,2], lty=2,col="red")
lines(TT1, conf.lines[,3], lty=2,col="purple")

lm(TT2~TT1)
summary(lm(TT2~TT1))

plot(TT1,K1)
abline(lm(K1~TT1), col="darkgreen", lwd=3)
cor(TT1,K1)
conf.lines<-predict(lm(K1~TT1), interval="confidence") 
lines(TT1, conf.lines[,2], lty=2,col="red")
lines(TT1, conf.lines[,3], lty=2,col="purple")

lm(K1~TT1)
summary(lm(K1~TT1))

plot(TM3,D3)
abline(lm(D3~TM3), col="darkgreen", lwd=3)
cor(TM3,D3)
conf.lines<-predict(lm(D3~TM3), interval="confidence") 
lines(TM3, conf.lines[,2], lty=2,col="red")
lines(TM3, conf.lines[,3], lty=2,col="purple")

lm(D3~TM3)
summary(lm(D3~TM3))
######################### regression #######################
lm(AFTER~BEFORE)
summary(lm(AFTER~BEFORE))

lm(sulyelso~suly)
summary(lm(sulyelso~suly))

######################## chi2 ###########################
str(kerd)
j<-table(nem,nehez)
j
chisq.test(j)
fisher.test(j)


k<-table(nem,orul)
k
chisq.test(k)
chisq.test(k,simulate.p.value=TRUE)
fisher.test(k)

z<-table(erdekes,orul)
z
chisq.test(z,simulate.p.value=TRUE)$expected
chisq.test(k,simulate.p.value=TRUE)
fisher.test(z)

gr<-table(gyakjegy,vizsga)
gr
fisher.test(gr)

#http://www.endmemo.com/program/R/pchsymbols.php