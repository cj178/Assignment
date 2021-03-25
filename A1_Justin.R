library(data.table)
library(dplyr)

# remove all existing R objects and set working directory
rm(list=ls()) 
setwd("D:/RJustin")

stu=read.csv("datstu.csv",header=T)
names(stu)
View(stu)

################E 1#############
#####number of students, schools, programs######
nrow(stu)

schoolcode=stu[,5:10]
View(schoolcode)
length(unique(as.vector(as.matrix(schoolcode))))


program=stu[,11:16]
view(program)
length(unique(as.vector(as.matrix(program))))
rm(program)
#####number of choices######

stu$choice1=paste0(stu$schoolcode1,stu$choicepgm1)
stu$choice2=paste0(stu$schoolcode2,stu$choicepgm2)
stu$choice3=paste0(stu$schoolcode3,stu$choicepgm3)
stu$choice4=paste0(stu$schoolcode4,stu$choicepgm4)
stu$choice5=paste0(stu$schoolcode5,stu$choicepgm5)
stu$choice6=paste0(stu$schoolcode6,stu$choicepgm6)
stucol=ncol(stu)
choice=stu[,(stucol-5):stucol]
View(choice)
length(unique(as.vector(as.matrix(choice))))

###################missing, same, less than 6############
missing=is.na(stu$score)
table(missing)

same=apply(as.matrix(schoolcode),1,function(x) length(unique(x)))
table(same)

table(is.na(stu$schoolcode6))

################E 2#############
sss=read.csv("datsss.csv",header=T)

ad=NULL
sturow=nrow(stu)
for(i in 1:sturow)
{if (!is.na(stu$rankplace[i])&stu$rankplace[i]<=6)
  {ad[i]=choice[i,stu$rankplace[i]]}
}

temp1=data.frame(stu$X,stu$score,stu$jssdistrict,ad)
names(temp1)=c("index","score","jssdistrict","ad")
View(temp1)

temp1$schoolcode=substr(temp1$ad,1,5)
temp2=merge(sss,temp1,by="schoolcode")
temp3=subset(temp2,select=c("index","score","ad","jssdistrict","schoolcode","sssdistrict","ssslong","ssslat"))


quality=aggregate(score~ad,temp1,mean)
cutoff=aggregate(score~ad,temp1,min)
names(quality)=c("ad","quality")
names(cutoff)=c("ad","cutoff")
temp4=merge(quality,cutoff,by="ad")

size=aggregate(index~ad,temp1,length)
names(size)=c("ad","size")
temp5=merge(temp4,size,by="ad")

temp6=merge(temp3,temp5,by="ad")
schinfo=unique(temp6)
View(schinfo)
rm(temp1)
rm(temp2)
rm(temp3)
rm(temp4)
rm(temp5)
rm(temp6)
rm(quality)
rm(cutoff)
rm(size)


schinfo1=na.omit(schinfo)
nrow(schinfo1)
rm(schinfo)

########E3#################

jss=read.csv("datjss.csv",header=T)
names(jss)=c("X","jssdistrict","jsslong","jsslat")
temp1=subset(jss,select=c("jssdistrict","jsslong","jsslat"))

distinfo=merge(temp1,schinfo1,by="jssdistrict")
rm(temp1)
distinfo$dist=sqrt((69.172*(distinfo$ssslong-distinfo$jsslong)*cos(distinfo$jsslat/57.3))^2+(69.172*(distinfo$ssslat-distinfo$jsslat))^2)






#####################part 2######################

# remove all existing R objects and set working directory
rm(list=ls()) 
setwd("D:/RJustin")


set.seed(10000)

# simulate independent variables
nobs=10000
x1=runif(nobs,min=1,max=3)
x2=rgamma(nobs,shape=3,scale=2)
x3=rbinom(nobs,1,0.3)
e=rnorm(nobs,mean=2,sd=1)

yhat=0.5+1.2*x1-0.9*x2+0.1*x3+e
yvar=as.numeric(yhat>0)

cor(yhat,x1)
X=cbind(1,x1,x2,x3)
b=solve(t(X)%*%X)%*%t(X)%*%yhat
b

###########nobs=50000############

set.seed(50000)

# simulate independent variables
nobs=50000
x1=runif(nobs,min=1,max=3)
x2=rgamma(nobs,shape=3,scale=2)
x3=rbinom(nobs,1,0.3)
e=rnorm(nobs,mean=2,sd=1)

yhat=0.5+1.2*x1-0.9*x2+0.1*x3+e
yvar=as.numeric(yhat>0)

X=cbind(1,x1,x2,x3)
b=solve(t(X)%*%X)%*%t(X)%*%yhat

###########nobs=100000############

set.seed(100000)

# simulate independent variables
nobs=100000
x1=runif(nobs,min=1,max=3)
x2=rgamma(nobs,shape=3,scale=2)
x3=rbinom(nobs,1,0.3)
e=rnorm(nobs,mean=2,sd=1)

yhat=0.5+1.2*x1-0.9*x2+0.1*x3+e
yvar=as.numeric(yhat>0)

X=cbind(1,x1,x2,x3)
data_temp=
b=solve(t(X)%*%X)%*%t(X)%*%yhat


############b converges to 1.2 as sample size increases#########################

############# sd ################

fisher_info=solve(2*t(X)%*%X)
sdb=sqrt(diag(fisher_info))

r1=cbind(b,sdb)
colnames(r1)=c("beta","standard error")
r1
############# check #############
reg=lm(yhat~x1+x2+x3)
summary(reg)


#############Excercise 7###############

###########linear########
N=100
sigma=1

flike=function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
    like           = N/2*log(sigma^2)+1/(2*sigma^2)*t(yvar-xbeta)%*%(yvar-xbeta)
  return(sum(like))
}


ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
  out[i0,] = res$par
}

mean_linear=apply(out,2,mean)
sd_linear=apply(out,2,sd)

mean_linear
sd_linear

#############Interpretation: intercept, b1, b2, and b3 significant at 0.01 level######################



###########probit########
flike=function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}


ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
  out[i0,] = res$par
}

mean_probit=apply(out,2,mean)
sd_probit=apply(out,2,sd)

##########test#############
probit=glm(yvar~x1+x2+x3,family=binomial(link=probit))
summary(probit)

#############Interpretation: intercept, b1, and b2 significant at 0.01 level, b3 not significant ######################

#####################logit#######################

flike=function(par,x1,x2,x3,yvar)
{
  beta=par
  pr              = exp(beta)/(1+exp(beta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}



ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
    res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
  out[i0,] = res$par
}

mean_logit=apply(out,2,mean)
sd_logit=apply(out,2,sd)

##########test#############

logit=glm(yvar~x1+x2+x3,family=binomial(link=logit))
summary(logit)

#############Interpretation: intercept, b1, and b2 significant at 0.01 level, b3 not significant ######################


####################Excercise 8 ###############################
lsamp=cbind(yvar,x1,x2,x3)
X=cbind(x1,x2,x3)

R    = 999;                      # number of bootstraps
nind = 10000;            # number of individuals
nvar = 4  # number of variables

outs = mat.or.vec(R,nvar)


for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = lsamp[samp,]
  reg1     = glm(yvar ~ x1+x2+x3,data = as.data.frame(dat_samp),family=binomial(link=probit))
  beta=as.vector(reg1$coefficients)
  outs[i,] = pnorm(beta)
}

mean_pst = apply(outs,2,mean)
sd_pst   = apply(outs,2,sd)
mean_pst
sd_pst
############logit###########


for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = lsamp[samp,]
  reg1     = glm(yvar ~ x1+x2+x3,data = as.data.frame(dat_samp),family=binomial(link=logit))
  beta=as.vector(reg1$coefficients)
  outs[i,] = exp(beta)/(1+exp(beta))
}

outs

mean_lst = apply(outs,2,mean)
sd_lst   = apply(outs,2,sd)
mean_lst
sd_lst


