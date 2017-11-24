for (package in c("bda","smoothmest")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

######## FIGURE 1: standard exponential curve

A=rexp(1000000, rate = 1)

setwd("C:/Users/Marie/Dropbox/ANOVA's Welch/Outputs of simulations/Figures/Figure 1")
png("Figure1.png",width=1800,height=900, res = 300)
plot(density(A),xlab="",main="")
dev.off()

######## FIGURE 2: The mixed normal distribution

A=rnorm(1000000,0,1)
B=rnorm(1000000,0,10)
C=rmixnorm(10000000,p=c(.9,.1),mean=rep(0,2),sd=c(1,10))

setwd("C:/Users/Marie/Dropbox/ANOVA's Welch/Outputs of simulations/Figures/Figure 2")

png("Figure2.png",width=2000,height=1200, res = 300)
plot(density(A),xlab="",main="",xlim=c(-20,20),ylim=c(0,.45))
lines(density(B),xlab="",main="",lty=2,xlim=c(-20,20))
lines(density(C),xlab="",main="",lty=3,lwd=3)
par(xpd=T)
legend(-20,.54,lty=c(1,2,3),lwd=c(1,1,3),legend=c("N(0,1)","N(0,10)","Mixed normal"),horiz=T,bty="n")
dev.off()

######## FIGURE 3: Asymetric distributions

A=rchisq(1000000,2)
B=-rchisq(1000000,2)

setwd("C:/Users/Marie/Dropbox/ANOVA's Welch/Outputs of simulations/Figures/Figure 3")

png("Figure3.png",width=2300,height=800, res = 300)
par(mfrow=c(1,2))
plot(density(B),xlab="",main="left skewed distribution",xlim=c(-12,0))
plot(density(A),xlab="",main="right skewed distribution",xlim=c(0,12))
dev.off()

######## FIGURE 4: p-value distributions for classical ANOVA F-test, Welch's W ANOVA and Brown-Forsythe F*-test

# reading useful files

read_sample=function(ssdossier="",k,distName,n,m,sds){
  # compute mean and standard deviation
  mu <- rep(0, k)
  std <- rep(0,k)
  distr <-  "Distr=["
  nobs <- "n=["
  means <-  "means=["
  stdevs <- "sds=["
  for (j in 1:k){
    if (j == k){
      distr <- paste0(distr, distName[j], "]")
      nobs <- paste0(nobs, n[j], "]")
      means <- paste0(means, m[j], "]")
      stdevs <- paste0(stdevs, sds[j], "]")
    } else {
      distr <- paste0(distr, distName[j], ",")
      nobs <- paste0(nobs, n[j], ",")
      means <- paste0(means, m[j], ",")
      stdevs <- paste0(stdevs, sds[j], ",")
    }
  }
  fname <-  paste(distr, nobs, means, stdevs, sep=",")
  setwd(dir=paste0("E:/Welch's W ANOVA/final test/Outputs/All simulations/k=",k,"/",ssdossier)) # destination file
  data=readRDS(file = paste0(fname,".rds"))
  return(data)
}

NOEFFECT_K2_N_n40_40_SDR4<-read_sample(ssdossier="without effect/normal/",k=2,distName=rep("normal",2),n=c(40,40),m=c(0,0),sds=c(2,8))
NOEFFECT_K3_N_n40_40_SDR4<-read_sample(ssdossier="without effect/normal/",k=3,distName=rep("normal",3),n=c(40,40,40),m=c(0,0,0),sds=c(2,2,8))

# graphs based on read files

graphique=function(name,var){
  setwd("C:/Users/Marie/Dropbox/ANOVA's Welch/Outputs of simulations/Figures/Figure 4")
  png(paste0(name,".png"),width=800,height=1500, res = 300)
  par(mfrow=c(3,1),cex.main=1.5,mar=c(5.1,4.1,4.1,2.1))
  hist(var[,1],main=paste("\n","ANOVA F test"),cex.main=1.5,xlab="Observed p-value",cex.lab=1.2,ylim=c(0,200000),cex.axis=1.1)
  hist(var[,2],main=paste("\n","Welch W test"),cex.main=1.5,xlab="Observed p-value",cex.lab=1.2,ylim=c(0,200000),cex.axis=1.1)
  hist(var[,3],main=paste("\n","Brown-Forsythe F* test"),cex.main=1.5,xlab="Observed p-value",cex.lab=1.2,ylim=c(0,200000),cex.axis=1.1)
  dev.off() 
}

graphique(name="NOEFFECT_K2_N_n40_40_SDR4",var=NOEFFECT_K2_N_n40_40_SDR4)
graphique(name="NOEFFECT_K3_N_n40_40_SDR4",var=NOEFFECT_K3_N_n40_40_SDR4)

######## FIGURE 5: comparing power of normal, doublex and mixed normal distribution

# reading needed files (see the reading file function)

EFFECT_K3_N_n20_20_20_SDR1<-read_sample(ssdossier="with effect/normal/",k=3,distName=rep("normal",3),n=c(20,20,20),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_N_n30_30_30_SDR1<-read_sample(ssdossier="with effect/normal/",k=3,distName=rep("normal",3),n=c(30,30,30),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_N_n40_40_40_SDR1<-read_sample(ssdossier="with effect/normal/",k=3,distName=rep("normal",3),n=c(40,40,40),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_N_n50_50_50_SDR1<-read_sample(ssdossier="with effect/normal/",k=3,distName=rep("normal",3),n=c(50,50,50),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_N_n100_100_100_SDR1<-read_sample(ssdossier="with effect/normal/",k=3,distName=rep("normal",3),n=c(100,100,100),m=c(0,0,1),sds=c(2,2,2))

EFFECT_K3_D_n20_20_20_SDR1<-read_sample(ssdossier="with effect/doublex/",k=3,distName=rep("doublex",3),n=c(20,20,20),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_D_n30_30_30_SDR1<-read_sample(ssdossier="with effect/doublex/",k=3,distName=rep("doublex",3),n=c(30,30,30),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_D_n40_40_40_SDR1<-read_sample(ssdossier="with effect/doublex/",k=3,distName=rep("doublex",3),n=c(40,40,40),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_D_n50_50_50_SDR1<-read_sample(ssdossier="with effect/doublex/",k=3,distName=rep("doublex",3),n=c(50,50,50),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_D_n100_100_100_SDR1<-read_sample(ssdossier="with effect/doublex/",k=3,distName=rep("doublex",3),n=c(100,100,100),m=c(0,0,1),sds=c(2,2,2))

EFFECT_K3_MX_n20_20_20_SDR1<-read_sample(ssdossier="with effect/mixed/",k=3,distName=rep("mixed",3),n=c(20,20,20),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_MX_n30_30_30_SDR1<-read_sample(ssdossier="with effect/mixed/",k=3,distName=rep("mixed",3),n=c(30,30,30),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_MX_n40_40_40_SDR1<-read_sample(ssdossier="with effect/mixed/",k=3,distName=rep("mixed",3),n=c(40,40,40),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_MX_n50_50_50_SDR1<-read_sample(ssdossier="with effect/mixed/",k=3,distName=rep("mixed",3),n=c(50,50,50),m=c(0,0,1),sds=c(2,2,2))
EFFECT_K3_MX_n100_100_100_SDR1<-read_sample(ssdossier="with effect/mixed/",k=3,distName=rep("mixed",3),n=c(100,100,100),m=c(0,0,1),sds=c(2,2,2))

# computing power 

power <- function(var=list()){
  power_results<-data.frame(matrix(0,5,5,dimnames=list(1:5,c("n1","n2","F-test","Welch","B-F"))))
  
  n1<-c(20,30,40,50,100)
  n2<-n1
  power_results[,1] =n1
  power_results[,2] =n2
  
  for (s in 1:5){
    power_results[s,3] = round(sum(var[[s]][,1]<.05)/length(var[[s]][,1]),3)
    power_results[s,4] = round(sum(var[[s]][,2]<.05)/length(var[[s]][,2]),3)
    power_results[s,5] = round(sum(var[[s]][,3]<.05)/length(var[[s]][,3]),3)
  }
  
  setwd(dir="C:/Users/Marie/Dropbox/ANOVA's Welch/Appendix/Output Figure 6 (à mettre sur disque dur)") # replace "???" by the desired destination file
  return(power_results)
}

normal_homobalanced=power(var=list(EFFECT_K3_N_n20_20_20_SDR1,EFFECT_K3_N_n30_30_30_SDR1,EFFECT_K3_N_n40_40_40_SDR1,EFFECT_K3_N_n50_50_50_SDR1,EFFECT_K3_N_n100_100_100_SDR1))
doublex_homobalanced=power(var=list(EFFECT_K3_D_n20_20_20_SDR1,EFFECT_K3_D_n30_30_30_SDR1,EFFECT_K3_D_n40_40_40_SDR1,EFFECT_K3_D_n50_50_50_SDR1,EFFECT_K3_D_n100_100_100_SDR1))
mixed_homobalanced=power(var=list(EFFECT_K3_MX_n20_20_20_SDR1,EFFECT_K3_MX_n30_30_30_SDR1,EFFECT_K3_MX_n40_40_40_SDR1,EFFECT_K3_MX_n50_50_50_SDR1,EFFECT_K3_MX_n100_100_100_SDR1))

View(mixed_homobalanced)

normal_F=normal_homobalanced[,3]
doublex_F=doublex_homobalanced[,3]
mixed_F=mixed_homobalanced[,3]

normal_W=normal_homobalanced[,4]
doublex_W=doublex_homobalanced[,4]
mixed_W=mixed_homobalanced[,4]

normal_BF=normal_homobalanced[,5]
doublex_BF=doublex_homobalanced[,5]
mixed_BF=mixed_homobalanced[,5]

setwd("C:/Users/Marie/Dropbox/ANOVA's Welch/Outputs of simulations/Figures/Figure 6")

png("power2.png",width=2000,height=3000,res=300)

par(mfrow=c(3,1),xpd=T)
plot(normal_F,type="b", ylim=c(.3,1),lty=1,axes=F,xlab="group sizes",ylab="power",pch=16,cex=.5,main="classical F ANOVA") #normal
lines(doublex_F, type="o", lty=2,pch=16,cex=.5) # doublex
lines(mixed_F, type="o", lty=3,pch=16,cex=.5) # mixed
axis(1, at=1:5, lab=c("20","30","40","50","100"))
axis(2, seq(.3,1,.1))
legend(1.5,1.1,c("normal","double exponential","mixed normal"),lty=c(1,2,3),bty="n",horiz=T)

plot(normal_W,type="b", ylim=c(.3,1),lty=1,axes=F,xlab="group sizes",ylab="power",pch=16,cex=.5,main="Welch's W ANOVA") #normal
lines(doublex_W, type="o", lty=2,pch=16,cex=.5) # doublex
lines(mixed_W, type="o", lty=3,pch=16,cex=.5) # mixed
axis(1, at=1:5, lab=c("20","30","40","50","100"))
axis(2, seq(.3,1,.1))
legend(1.5,1.1,c("normal","double exponential","mixed normal"),lty=c(1,2,3),bty="n",horiz=T)

plot(normal_BF,type="b", ylim=c(.3,1),lty=1,axes=F,xlab="group sizes",ylab="power",pch=16,cex=.5,main="Brown-Forsythe F* ANOVA") #normal
lines(doublex_BF, type="o", lty=2,pch=16,cex=.5) # doublex
lines(mixed_BF, type="o", lty=3,pch=16,cex=.5) # mixed
axis(1, at=1:5, lab=c("20","30","40","50","100"))
axis(2, seq(.3,1,.1))
legend(1.5,1.1,c("normal","double exponential","mixed normal"),lty=c(1,2,3),bty="n",horiz=T)

dev.off()

