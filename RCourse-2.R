setwd("/media/store/works/teaching/biostatisztika/2")
setwd("/media/data/Agi/biostatisztika/2")
setwd("/media/store/works/teaching/biostatisztika/bevezeto")

############################## hf ###########################################
d<-read.csv("csaszar.csv")
d
str(d)
summary(d)
attach(d)
detach(d)
is.vector(pH)
is.factor(elektiv.mutet)
y<-as.character(pH)
y
pO2[3]
pCO2[c(2,5,12)]
pH[4:10]
HCO3[HCO3>20]
hist(pH,col="darkblue",xlab="pH",ylab="freq",main="pH frequencies")
table(elektiv.mutet)
barplot(table(elektiv.mutet),col="seagreen",xlab="mutet",ylab="freq",main="elektiv mutetek gyakorisaga")

############################### TABLE OF DESCRIPTIVE STATISTICS ################################

csaszar_table<-read.csv("csaszar.csv")
csaszar_table
attach(csaszar_table)
detach(csaszar_table)
csaszar<-as.data.frame(csaszar_table[1:6])

l<-lapply(csaszar,length)
xbar<-lapply(csaszar, mean)
s<-lapply(csaszar,sd)
med<-lapply(csaszar, median)
minimum<-lapply(csaszar, min)
maximum<-lapply(csaszar, max)
#rang<-(maximum-minimum)
q25<-lapply(csaszar, quantile,0.25, names=F)
q75<-lapply(csaszar, quantile,0.75, names=F)
cbind(mean=xbar, st.dev=s,minimum,q25,median=med,q75, maximum)

###################### even distribution ######################
b<-sample(1:80,1000,replace=T)
mean(b)
summary(b)
hist(b)
boxplot(b)
efv_b <- ecdf(b)
plot(efv_b)
qqnorm(b); qqline(b)


######### normal distribution ######################################
a<-rnorm(1000,m=50,sd=10)
a
hist(a,col="red",main="histogram of a")
plot(density(a),main="Density estimate of data")

efv_a <- ecdf(a)
plot(efv_a)

mean(a)
median(a)
sd(a)
summary(a)
boxplot(a)

qqnorm(a); qqline(a)

########################## normally distributed sample ####################################################x
c<-read.csv("leveltetu.csv")
c
attach(c)
hist(x,col="red",main="histogram of c")
is.data.frame(c)
dim(c)
qqnorm(x); qqline(x)
ks.test(x, "pnorm", 20, 12.21)       # biostat konyv 7.3.1 B,C fejezetek
plot(density(x),main="Density estimate of c")
efv_c <- ecdf(x)
plot(efv_c)


png(file="hisztogram_a.png")
hist(a,col="lavenderblush3",main="histogram of a",freq=TRUE)
dev.off()



#################### patch size data ###############################
foltok<-read.csv("mindenfolt.csv",header=TRUE)
str(foltok)
attach(foltok)
detach(foltok)
is.vector(kitettseg)
is.factor(szelveny)
Fkit<-as.factor(kitettseg)
Fszelv<-as.factor(szelveny)
summary(szelveny)
summary(Fszelv)
barplot(table(Fkit))
barplot(table(Fszelv))
hist(foltmeret,col="orange",breaks=60)
efv_foltmeret <- ecdf(foltmeret)
plot(efv_foltmeret)
boxplot(foltmeret)
plot(density(foltmeret))
qqnorm(foltmeret); qqline(foltmeret)



