rm(list=ls())
library(bayesm)
library(ggplot2)
library(cowplot)
library(dplyr)

#==========================================
# exercise 1:  
#==========================================

data(margarine)
print(table(margarine$choicePrice[,2]))
mean=apply(as.matrix(margarine$choicePrice[,3:12]), 2, mean)
var=apply(as.matrix(margarine$choicePrice[,3:12]), 2, var)

freq<-as.numeric(table(margarine$choicePrice[,2]))
share<-NULL
share[1]<-freq[1]/sum(freq[1:10])
share[2]<-freq[2]/sum(freq[1:10])
share[3]<-freq[3]/sum(freq[1:10])
share[4]<-freq[4]/sum(freq[1:10])
share[5]<-freq[5]/sum(freq[1:10])
share[6]<-freq[6]/sum(freq[1:10])
share[7]<-freq[7]/sum(freq[1:10])
share[8]<-freq[8]/sum(freq[1:10])
share[9]<-freq[9]/sum(freq[1:10])
share[10]<-freq[10]/sum(freq[1:10])

mat<-cbind(mean,share)
order<-mat[order(mat[,1]),]
mat1<-as.data.frame(order[1:5,])
mat2<-as.data.frame(order[6:10,])

p1<-ggplot(mat1,aes(mean,share))+geom_smooth()
p2<-ggplot(mat2,aes(mean,share))+geom_smooth()
plot_grid(p1,p2)

info=merge(margarine$choicePrice,margarine$demos,by="hhid")
summary(info)

mapping=info%>%
  select(2,13:18) %>% 
  group_by(choice) %>% 
  summarize_each(funs(mean))

rm(order,mat,mat1,mat2)
#==========================================
# exercise 2 3 4
#==========================================
########################conditional logit ########################

price=select(info,3:12)  
choice=info$choice
mi = nrow(info)
mj = length(unique(choice))

param=NULL
L2=function(param,info)
{
  ut=mat.or.vec(mi,mj)
  for (j in 1:mj)
  {
    ut[,j] = param[1] + param[2]*price[,j]
  }
  prob   = exp(ut)  # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))   
  probc = NULL
  
  for (i in 1:mi)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}
####optim######
startCL= runif(2,-5,5)
res_CL   = optim(startCL,fn=L2,method="BFGS",info=info,hessian=TRUE)

####marginal effect#####
param = res_CL$par
outs=NULL
for (i in 1:mi)
{
  ut=mat.or.vec(mi,mj)
  for (j in 1:mj)
  {
    ut[,j] = param[1] + param[2]*price[,j]
  }
  prob   = exp(ut)  # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))  
  
  outs[i]=prob[i,choice[i]]*(1-prob[i,choice[i]])*param[2]
}

margeff_CL=mean(outs)
margeff_CL


################### multinomial logit ########################
choice=info$choice
income=info$Income

ni = nrow(info)
nj = length(unique(choice))

par=NULL
L1=function(par,info)
{
  u=mat.or.vec(ni,nj)
  pn1    = par[1:nj]
  pn2    = par[(nj+1):(2*nj)]
  
  for (j in 1:nj)
  {
    u[,j] = pn1[j]+income*pn2[j] 
    }
  prob   = exp(u)  # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))   
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

#########optim#############
startMNL= runif(20,-5,5)
res_MNL  = optim(startMNL,fn=L1,method="BFGS",info=info,hessian=TRUE)


#########marginal effect#############

pn1=res_MNL$par[1:10]
pn2 = res_MNL$par[11:20]
out=NULL
betas=NULL
prob=NULL

for (i in 1:ni)
{
  u=mat.or.vec(ni,nj)
for (j in 1:nj)
{
  u[,j] = pn1[j]+income*pn2[j] 
  betas[i]=prob[i,j]*pn2[j]
}
prob   = exp(u)  # exp(XB)
prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))   
probc = NULL

out[i]=prob[i,choice[i]]*(pn2[choice[i]]-betas[i])
}

margeff_MNL=mean(out)
margeff_MNL

#==========================================
# exercise 5  
#==========================================


#######mixed################# 
income=info$Income
price=select(info,3:12) 
parmix=NULL

L3=function(parmix,info)
{
  um=mat.or.vec(ni,nj)
  pn1    = parmix[1:nj]
  pn2    = parmix[(nj+1):(2*nj)]
  
  for (j in 1:nj)
  {
    um[,j] = pn1[j]+income*pn2[j]+price[,j]*parmix[21] 
  }
  prob   = exp(um)  # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))   
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

startM= runif(21,-5,5)
res_M   = optim(startM,fn=L3,method="BFGS",info=info,hessian=TRUE)
betaf=res_M$par

Lrf=L3(betaf,info = info)




parmix=NULL

info1=info[with(info,choice!=10),]
info2=dplyr::select(info1,-12)
choice2=info2$choice
income2=info2$Income
price2=select(info2,3:11) 


njr=length(unique(choice2))
nir=nrow(info2)


L4=function(parmix,info)
{
  um=mat.or.vec(nir,njr)
  pn1    = parmix[1:njr]
  pn2    = parmix[(njr+1):(2*njr)]
  
  for (j in 1:njr)
  {
    um[,j] = pn1[j]+income2*pn2[j]+price2[,j]*parmix[19] 
  }
  prob   = exp(um)  # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))   
  probc = NULL
  for (i in 1:nir)
  {
     probc[i] = prob[i,choice2[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

startMR= runif(19,-5,5)
res_MR= optim(startMR,fn=L4,method="BFGS",info=info2,hessian=TRUE)
betaR=res_MR$par
Lrr=L4(betaR,info = info2)
chi=-2*(Lrf-Lrr)
