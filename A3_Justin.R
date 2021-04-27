rm(list=ls())
library(dplyr)
library(ggplot2)
library(AER)

#==========================================
# exercise 2:  
#==========================================
setwd("D:/RJustin")
pop=read.csv("population.csv",header=T)
crim=read.csv("crime_long.csv",header=T)
names(crim)=c("month","district","type","count")

#total crime per month and plot#
ncrim=crim%>% 
  group_by(month) %>% 
  summarise(n=sum(count))

p1<-ggplot(ncrim,aes(x=as.numeric(month),y=n,group=1))+geom_line()+ xlab("Month") + ylab("Violent Crime")+theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.01))
p1

#merge crime and population#
crim1=aggregate(count~ month+district+type,data=crim,sum)
info=merge(pop,crim1,by=c("month","district"))

#create new variables#
info$pcrim=info$count/info$tot_pop
info$share_w=info$tot_white/info$tot_pop
info$share_b=info$tot_black/info$tot_pop
info$share_h=info$tot_hisp/info$tot_pop

infov=subset(info,type=="violent",select=c("month","district","pcrim"))%>% 
  rename(c(pviol="pcrim"))
infop=subset(info,type=="property",select=c("month","district","pcrim"))%>% 
  rename(c(pprop="pcrim"))

info2=merge(info,infov,by=c("month","district"))
info3=merge(info2,infop,by=c("month","district"))

info4=info3%>% 
  group_by(month,district) %>% 
  mutate(ptcrim=sum(pcrim))

dinfo=info4%>% dplyr::select(month,district,ptcrim,pviol,pprop,p50_inc,share_b,share_h,share_w)
data=unique(dinfo)

rm(crim)
rm(crim1)
rm(dinfo)
rm(info)
rm(infop)
rm(infov)
rm(info2)
rm(info3)
rm(info4)
rm(ncrim)
rm(pop)


#==========================================
# exercise 3 and 4:  
#==========================================
offi=read.csv("officers.csv",header=T)
names(offi)=c("id","month","district","tenure","arrest")
data3=merge(data,offi,by=c("month","district"))%>% 
  rename(c(date="month"))

data3$year=substr(data3$date,1,4)
data3$month=substr(data3$date,6,7)

e3=lm(arrest~tenure+ptcrim+p50_inc+share_b+share_h+share_w-1, data=data3)
coeftest(e3, vcov = sandwich)

e4=lm(arrest~tenure+ptcrim+p50_inc+share_b+share_h+share_w+factor(district)+year+month-1, data=data3)
coeftest(e4, vcov = sandwich)
printCoefmat(coeftest(e4, vcov = sandwich)[1:6,])

#==========================================
# exercise 5:  
#==========================================

data5=data3%>%
  group_by(id,district)%>% 
  mutate(am=mean(arrest),tm=mean(tenure),cm=mean(ptcrim),im=mean(p50_inc),bm=mean(share_b),hm=mean(share_h),wm=mean(share_w)) %>%
  mutate(ad=arrest-am,td=tenure-tm,cd=ptcrim-cm,id=p50_inc-im,bd=share_b-bm,hd=share_h-hm,wd=share_w-wm)%>%
  mutate(a1=lag(arrest, n = 1, default = NA),
       t1=lag(tenure, n = 1, default = NA),
       c1=lag(ptcrim, n = 1, default = NA),
       i1=lag(p50_inc, n = 1, default = NA),
       b1=lag(share_b, n = 1, default = NA),
       h1=lag(share_h, n = 1, default = NA),
       w1=lag(share_w, n = 1, default = NA))

data55=data5%>%
  na.omit()%>%
  mutate(afd=arrest-a1,tfd=tenure-t1,cfd=ptcrim-c1,ifd=p50_inc-i1,bfd=share_b-b1,hfd=share_h-h1,wfd=share_w-w1)

####between estimator#####
bet=lm(am~tm+cm+im+bm+hm+wm+factor(district),data=data5)
printCoefmat(coeftest(bet, vcov = sandwich)[1:7,])

####within estimator#####
wit=lm(ad~td+cd+id+bd+hd+wd+factor(district),data=data5)
printCoefmat(coeftest(wit, vcov = sandwich)[1:7,])

####first difference estimator#####
fd=lm(afd~tfd+cfd+ifd+bfd+hfd+wfd+factor(district),data=data55)
printCoefmat(coeftest(wit, vcov = sandwich)[1:7,])



library(plm)
formula = arrest~tenure+ptcrim+p50_inc+share_b+share_h+share_w
#+factor(district)+year+month-1
model1 = plm(formula, data=data3, model="within")
model2 = plm(formula, data=Produc, model="between")
model3 = plm(formula, data=data3, model="fd")



###### GMM #####
library(moments)
library(tidyverse)

set.seed(123)
mom = all.moments(data3$arrest,order.max=9)
mom[-1]

x1=as.numeric(data3$tenure)
x2=data3$ptcrim
x3=data3$p50_inc
x4=data3$share_b
x5=data3$share_h
x6=data3$share_w
x7=as.numeric(data3$district)
x8=as.numeric(data3$year)
x9=as.numeric(data3$month)



# method of moments with simulations here
mm_sim = function(par,emp_mom)
{
  nsim=50
  sim_mom = mat.or.vec(9,nsim)
  for (iS in 1:nsim)
  {
    # form the moment condition
    xbeta= par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3+par[5]*x4 + par[6]*x5 + par[7]*x6+ par[8]*x7 + par[9]*x8+ par[10]*x9
    # calculate the moments
    sim_mom[,iS] = all.moments(xbeta,order.max =9)[-1]
  }
  sim_mom = apply(sim_mom,1,mean)
  like = sum((sim_mom - emp_mom)^2)
  return(like);
}

start = c(100,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
res  = optim(start,fn=mm_sim,method="BFGS",control=list(trace=10,REPORT=1,maxit=1000),emp_mom=mom[-1])


