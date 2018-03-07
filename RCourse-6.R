setwd("/media/SAMSUNG/MY/works/teaching/biostatisztika/5")

a<-read.csv("antropometria.csv")
str(a)
attach(a)
detach(a)
plot(TM1,TM2)
abline(lm(TM2~TM1), col="darkgreen", lwd=3)
cor(TM1,TM2)
conf.lines<-predict(lm(TM2~TM1), interval="confidence") 
lines(TM1, conf.lines[,2], lty=2,col="red")
lines(TM1, conf.lines[,3], lty=2,col="purple")

plot(D_mean,TT_mean)
abline(lm(TT_mean~D_mean), col="darkgreen", lwd=3)
cor(D_mean,TT_mean)
conf.lines<-predict(lm(TT_mean~D_mean), interval="confidence") 
lines(D_mean, conf.lines[,2], lty=2,col="red")
lines(D_mean, conf.lines[,3], lty=2,col="red")
lm(TT_mean~D_mean)
summary(lm(TT_mean~D_mean))

kerd<-read.csv("kerd98p.csv")
attach(kerd)
detach(kerd)
s<-table(nem,erdekes)
s
chisq.test(s)
chisq.test(s,simulate.p.value=TRUE)
fisher.test(s)


k<-table(nem,enni)
k
z<-table(nem, ismer)
z
chisq.test(z)
chi<-chisq.test(k,simulate.p.value=TRUE)
fisher.test(k)
chi$observed
chi$expected

################################## anova ###################################

vars<-as.data.frame(cbind(suly, enni, hajszin, szemszin))
vars
plot(vars)

# one way
enni<-(factor(enni))
boxplot(suly~enni)
fit<-aov(suly~enni)
fit
summary(fit)
pairwise.t.test(suly,enni,p.adj="none")
pairwise.t.test(suly,enni,p.adj="bon")
t.test

aovmod1=lm(suly~enni)
anova(aovmod1)
summary(aovmod1)

#### two way
szemszin<-factor(szemszin)
hajszin<-factor(hajszin)
TWA<-aov(magassag~hajszin*szemszin)
TWA
summary(TWA)
pairwise.t.test(suly,szemszin,p.adj="none")
pairwise.t.test(suly,hajszin,p.adj="none")

############################ cluster #######################################
cars<-read.csv("cars.csv")
head(cars)

#Suppose we want to standardize by subtracting the median and dividing by the mean average deviation:
cars.use = cars[,-c(1,2)]
medians = apply(cars.use,2,median)
mads = apply(cars.use,2,mad)
cars.use = scale(cars.use,center=medians,scale=mads)

#calculating a distance matrix
cars.dist = dist(cars.use)     #library(cluster) - daisy() is even better
cars.dist                      # as.matrix to convert it to a regular matrix. 

#calculating the clusters
cars.hclust = hclust(cars.dist)    #library(cluster) - agnes()
plot(cars.hclust,main='Default from hclust')
#plot(cars.hclust,labels=cars$Car,main='Default from hclust')

########################### test ##################################
a<-read.csv("antropometria.csv")
str(a)
attach(a)
summary(a)
detach(kerd)
#create table
TT<-TT_mean[TM_mean>166 & TM_mean<170]
TM<-TM_mean[TM_mean>166 & TM_mean<170]
CS<-CS_K[TM_mean>166 & TM_mean<170]
D<-D_mean[TM_mean>166 & TM_mean<170]
D
mytable<-as.data.frame(cbind(TT,TM,D,CS))
mytable

first<-cbind(tt=mean(TT1),tm=mean(TM1),d=mean(D1))
sec<-cbind(sd(TT1),sd(TM1),sd(D1))
Table<-as.data.frame(rbind(first,sec))
first

#descriptive statistics
hist(TT_mean,col="navajowhite",main="testtömegek átlaga"
     ,xlab="testtömeg",ylab="gyakoriságok")
plot(density(TT_mean))
plot(ecdf(TT_mean))
boxplot(TT_mean,col="navajowhite",main="boxdiagram a testtömegekrõl")
qqnorm(TT_mean);qqline(TT_mean) 
table(nem)
barplot(table(nem))

#correlation
plot(K3,D3)
abline(lm(D3~K3), col="darkred", lwd=3)
cor(K3,D3)
conf.lines<-predict(lm(D3~K3), interval="confidence") 
lines(K3, conf.lines[,2], lty=2,col="red")
lines(K3, conf.lines[,3], lty=2,col="red")

lm(D3~K3)
summary(lm(D3~K3))

#tests
t.test(TM1,TM2,paired=TRUE)

t.test(TT_mean[nem==1],TT_mean[nem==2])
boxplot(TT_mean[nem==1],TT_mean[nem==2])

chisq.test(nem,enni)
fisher.test(nem,enni)

