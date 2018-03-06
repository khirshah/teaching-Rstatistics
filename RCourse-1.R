#################### set working directory ##################################
setwd("/media/store/works/teaching/biostatisztika/bevezeto")
dir()

###################### use as calculator ############################
4+12
7-3
5*6
4/2
2^8
5+2^8
(5+2)^8

############################## functions and help ##############################

?log
?mean
############################# assignment operator ##############################
x<-8
x<-8L
x<-"hello world!"
x<-TRUE
x<-8+2i
x
############################# data types and containers ###################

class(x)
as.numeric(x)  ## conversion
y<-as.logical(x)
y

## vectors
?vector()
Avektor<-c(5,6,2,4,8)
str(Avektor)
class(Avektor)
is.vector(Avektor)
Bvektor<-c(1:20)
Bvektor<-c(as.numeric(1:20))
class(Bvektor)
str(Bvektor)
Avektor
Bvektor
Cvektor<-c("hi","hello","seeya")
Dvektor<-c(Avektor,Bvektor,Dvektor)
Dvektor
class(Dvektor)
str(Dvektor)

Avektor[1]                   # simple selector
Avektor[2:3]                 # sequence selector
Avektor(c(2,4))              # vector selector
Avektor[Avektor>3]           #logical selector

##lists
mylist<-list(2,TRUE,"Peter",84.321,FALSE)
mylist
class(mylist)
str(mylist)
summary(mylist)

## factors
?factor

jegyek<-c(5,2,1,4,3,1,2,5)  # create a vector!
grades<-factor(jegyek)      # convert it to factor
as.factor(jegyek)           # this is also valid conversion
class(jegyek)
class(grades)
table(grades)

nemek<-(factor(c("Male","Female","Female","Male")))  # vector creation and conversion in one step
nemek
str(nemek)
table(nemek)
class(nemek)

## matrices
?matrix()
M<-matrix(1:50,nrow=5,ncol=10)
dim(M)

vektor1<-c(1:10)
vektor2<-c(10:20)
mat<-cbind(vektor1,vektor1)
mat
rbind(vektor1,vektor2)
class(mat)

## data frames (file import)
?data.frame()


############################### data import #####################################x
c<-read.table("leveltetu.txt",header=TRUE)
c
str(c)
d<-read.csv("leveltetu.csv")
str(d)

attach(d)
detach(d)

rm(x) # remove/delete
sort(x)

cirok<-(read.csv("cirok.csv"))
cirok
str(minta)
str(cirok$minta)
attach(cirok)
str(minta)
summary(minta)

kerdoiv<-(read.csv("kerdoiv.csv"))
str(kerdoiv)
kerdoiv
attach(kerdoiv)

######################## basic statistical functions ###################################
summary(x)
is.data.frame(d)
is.vector(x)
dim(d)

mean(d$x)
mean(x)
median(x)
sd(x)

x
table(x)

######################## plots ######################################
?hist
plot.new()
hist(x,col="blue")
hist(minta)
hist(ppm)
hist(mmol.g)

Nem
barplot(table(Nem))
barplot(table(Kor))
?png
png("histogram.png",width = 480, height = 480, units = "px")
hist(x,col="indianred")
dev.off()
