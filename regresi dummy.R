#import data from excel
data=baru
data
a<-data$CGPA
b<-data$`TOEFL Score`
c<-data$`GRE Score`
e<-data$`Chance of Admit`
d<-data$Research

summary(a)
summary(b)
summary(c)
summary(d)
summary(e)

scatter.smooth(a,e)
scatter.smooth(b,e)
scatter.smooth(c,e)
scatter.smooth(d,e)

cor(a,e,method="pearson")
cor(b,e,method="pearson")
cor(c,e,method="pearson")
cor(d,e,method="pearson")

y=as.matrix(data[,5])
x=as.matrix(data[,1:4])
x0<-matrix(1,nrow=162)
X=cbind(x0,x)

library("lmtest")
library(MASS)
library(faraway)
cal.weights<-(1/(lm(abs(regresi$residuals)~regresi$fitted.values)$fitted.values)^2)
cal.lmw<-lm(e~a+b+c+d,data=data,weights=cal.weights)
summary(cal.lmw)
anova(cal.lmw)

##################################
#NON MULTIKOLINEARITAS#
##################################
multicol=function(X){
  VIF=diag(solve(cor(X)))
  result=ifelse(VIF>5,"mulicolinearity", "non multicolinearity")
  data1=data.frame(VIF,result)
  return(data1)
}
multicol(x)

##################################
#UJI ASUMSI LINEARITAS#
##################################
resettest(cal.lmw)

#H0 : model bersifat linier

##################################
#UJI ASUMSI NORMALITAS#
##################################

library(car)
library(MASS)
boxplot(a)
boxplot(b)
boxplot(c)
boxplot(d)
boxplot(y)

# Normality of Residuals
# qq plot for studentized resid
outlierTest(cal.lmw)
qqPlot(cal.lmw1, main="QQ Plot")
# distribution of studentized residuals
sresid<-studres(cal.lmw)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
ks.test(sresid, "pnorm")
#H0 : residual data bersifat normal

##################################
#ASUMSI NON HETEROSKEDASTISITAS#
##################################
library(car)
library(MASS)
##heteroskedastisitas test
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(cal.lmw)

# plot studentized residuals vs. fitted values
spreadLevelPlot(cal.lmw)

#H0 : variansi residual bersifat homoskedastisitas 

##################################
#UJI ASUMSI NON AUTOKORELASI#
##################################
#uji breusch godfey
bgtest(cal.lmw)
