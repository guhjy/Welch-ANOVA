# read needed files (see the reading script)

NOEFFECT_K3_N_n20_20_20_SDR1<-read_sample(ssdossier="without effect/normal/",k=3,distName=rep("normal",3),n=c(20,20,20),m=c(0,0,0),sds=c(2,2,2))
NOEFFECT_K3_D_n20_20_20_SDR1<-read_sample(ssdossier="without effect/doublex/",k=3,distName=rep("doublex",3),n=c(20,20,20),m=c(0,0,0),sds=c(2,2,2))
NOEFFECT_K3_DC_n20_20_20_SDR1<-read_sample(ssdossier="without effect/doublex non corrigé/",k=3,distName=rep("doublex",3),n=c(20,20,20),m=c(0,0,0),sds=c(2.828,2.828,2.828))
NOEFFECT_K3_MX_n20_20_20_SDR1<-read_sample(ssdossier="without effect/mixed/",k=3,distName=rep("mixed",3),n=c(20,20,20),m=c(0,0,0),sds=c(2,2,2))
NOEFFECT_K3_KPOS_n20_20_20_SDR1<-read_sample(ssdossier="without effect/chi² and skewpos/",k=3,distName=c("chi2","chi2","skewpos"),n=c(20,20,20),m=c(2,2,2),sds=c(2,2,2))
NOEFFECT_K3_KNEG_n20_20_20_SDR1<-read_sample(ssdossier="without effect/chi² and skewneg/",k=3,distName=c("chi2","chi2","skewneg"),n=c(20,20,20),m=c(2,2,2),sds=c(2,2,2))
NOEFFECT_K3_ES_n20_20_20_SDR1<-read_sample(ssdossier="without effect/skewpos,skewpos/",k=3,distName=c("skewpos","skewpos","skewpos"),n=c(20,20,20),m=c(0,0,0),sds=c(2,2,2))
NOEFFECT_K3_IS_n20_20_20_SDR1<-read_sample(ssdossier="without effect/skewpos,skewneg/",k=3,distName=c("skewpos","skewpos","skewneg"),n=c(20,20,20),m=c(0,0,0),sds=c(2,2,2))

sd_normal<-NOEFFECT_K3_N_n20_20_20_SDR1[,8]
sd_doublex<-NOEFFECT_K3_D_n20_20_20_SDR1[,8]
sd_doublexC<-NOEFFECT_K3_DC_n20_20_20_SDR1[,8]
sd_mixed<-NOEFFECT_K3_MX_n20_20_20_SDR1[,8]
sd_k<-NOEFFECT_K3_KPOS_n20_20_20_SDR1[,8]
sd_skewpos<-NOEFFECT_K3_ES_n20_20_20_SDR1[,8]
sd_skewneg<-NOEFFECT_K3_IS_n20_20_20_SDR1[,9] # col 8 = skewpos, col 9 = skewneg

# relation between the sampling distribution of variance and chi² distribution

sigma2=4
link_normal<-(19*sd_normal^2)/sigma2  # (n-1)S²/sigma²
link_doublex<-(19*sd_doublex^2)/sigma2 # (n-1)S²/sigma²
link_doublexC<-(19*sd_doublexC^2)/sigma2 # (n-1)S²/sigma²
link_mixed<-(19*sd_mixed^2)/sigma2 # (n-1)S²/sigma²
link_k<-(19*sd_k^2)/sigma2 # (n-1)S²/sigma²
link_skewpos<-(19*sd_skewpos^2)/sigma2 # (n-1)S²/sigma²
link_skewneg<-(19*sd_skewneg^2)/sigma2 # (n-1)S²/sigma²

# expected sampling distribution of variance

K <- rchisq(1000000,df=19)

setwd("C:/Users/Marie/Dropbox/ANOVA's Welch/Appendix/Figures used in Appendixes/Appendix 2_sampling distribution of mean and var")

png("FigureA2.1.png",width=2000,height=1200, res = 300)
plot(density(link_normal),xlab="",main="",ylim=c(0,0.08))
lines(density(K),lty=2,lwd=2)
abline(v=mean(link_normal),lty=1,col="red",lwd=2)
par(xpd=T)
legend(-2,0.098,legend=c("observed sampling distribution of ((n-1)s²)/sigma² ","chi(n-1)"),lwd=c(1,2),lty=c(1,2),horiz=T,bty="n")
dev.off()

LOW<-sum(K<19)/length(K) # .543365
UP<-sum(K>19)/length(K) # .456635

#--------------------------------------------------------

png("FigureA2.2.png",width=2000,height=1200, res = 300)
plot(density(link_doublex),xlab="",main="",ylim=c(0,0.08))
#points(13.145,0,pch="l")
par(xpd=T)
lines(density(K),lty=2,lwd=2)
par(xpd=F)
abline(v=13.145,lty=1)
abline(v=28.905,lty=2)
abline(v=mean(link_doublex),lty=1,col="red",lwd=2) # ajouter la valeur des lignes verticales
par(xpd=T)
legend(-8,0.098,legend=c("observed sampling distribution of ((n-1)s²)/sigma² ","chi(n-1)"),lwd=c(1,2),lty=c(1,2),horiz=T,bty="n")
dev.off()

A<-sum(K<13.145)/length(K) # .16893
B<-sum(link_doublex<13.145)/length(link_doublex) #.297067

B/A # 1.759

C<-sum(K>28.905)/length(K) #.067488
D<-sum(link_doublex>28.905)/length(link_doublex) #.134699

D/C# D/C = 1.996

LOW<-sum(link_doublex<19)/length(K) # .586078 vs .543365 (in normal case) 
UP<-sum(link_doublex>19)/length(K) # .413922 vs .456635  (in normal case)

#--------------------------------------------------------

png("FigureA2.3.png",width=2000,height=1200, res = 300)
plot(density(link_mixed),xlab="",main="",ylim=c(0,0.08)) 
lines(density(K),lty=2,lwd=2)
par(xpd=F)
abline(v=12.1)
abline(v=30.75,lty=2)
abline(v=mean(link_mixed),lty=1,col="red",lwd=2)
par(xpd=T)
legend(-15,0.098,legend=c("observed sampling distribution of ((n-1)s²)/sigma² ","chi(n-1)"),lwd=c(1,2),lty=c(1,2),horiz=T,bty="n")
dev.off()

A<-sum(K<12.1)/length(K) #.118416
B<-sum(link_doublex<12.1)/length(link_doublex) #.242888

B/A # 2.051

C<-sum(K>30.75)/length(K) #.042931
D<-sum(link_doublex>30.75)/length(link_doublex) #.107888

D/C# D/C = 2.513

LOW<-sum(link_mixed<19)/length(K) # .639266 vs .543365 (in normal case) 
UP<-sum(link_mixed>19)/length(K) # .360734 vs .456635  (in normal case)

#--------------------------------------------------------

png("FigureA2.4.png",width=2000,height=1200, res = 300)
plot(density(link_skewpos),xlab="",main="",ylim=c(0,0.08)) #
lines(density(K),lty=2,lwd=2)
par(xpd=F)
abline(v=13.555)
abline(v=27.382,lty=2)
abline(v=mean(link_skewpos),lty=1,col="red",lwd=2)
par(xpd=T)
legend(-4,0.098,legend=c("observed sampling distribution of ((n-1)s²)/sigma² ","chi(n-1)"),lwd=c(1,2),lty=c(1,2),horiz=T,bty="n")
dev.off()

A<-sum(K<13.555)/length(K) #.191021
B<-sum(link_doublex<13.555)/length(link_doublex) #.318803

B/A # 1.669

C<-sum(K>27.382)/length(K) #.096052
D<-sum(link_doublex>27.382)/length(link_doublex) #.16168

D/C# D/C = 1.683

LOW<-sum(link_skewpos<19)/length(K) # .558534 vs .543365 (in normal case) 
UP<-sum(link_skewpos>19)/length(K) # .441466 vs .456635  (in normal case)

#--------------------------------------------------------

png("FigureA2.5.png",width=2000,height=1200, res = 300)
plot(density(link_skewneg),xlab="",main="",ylim=c(0,0.08),xlim=c(0,100)) #
lines(density(K),lty=2,lwd=2)
par(xpd=F)
abline(v=13.55)
abline(v=28.625,lty=2)
abline(v=mean(link_skewneg),lty=1,col="red",lwd=2)
par(xpd=T)
legend(-7,0.37,legend=c("observed sampling distribution of ((n-1)s²)/sigma² ","chi(n-1)"),lwd=c(1,2),lty=c(1,2),horiz=T,bty="n")
dev.off()

A<-sum(K<13.55)/length(K) #.190731
B<-sum(link_doublex<13.55)/length(link_doublex)  #.318551

B/A # 1.670158

C<-sum(K>28.625)/length(K) #.072153
D<-sum(link_doublex>28.625)/length(link_doublex) #.139296

D/C# D/C = 1.930564

LOW<-sum(link_skewneg<19)/length(K) # .558806 vs .543365 (in normal case) 
UP<-sum(link_skewneg>19)/length(K) # .441194 vs .456635  (in normal case)

#--------------------------------------------------------

png("FigureA2.6.png",width=2000,height=1200, res = 300)
plot(density(link_k),xlab="",main="",ylim=c(0,0.08),xlim=c(0,70)) #
lines(density(K),lty=2,lwd=2)
par(xpd=F)
abline(v=12.72)
abline(v=29.9,lty=2)
abline(v=mean(link_k),lty=1,col="red",lwd=2)
par(xpd=T)
legend(-4,0.098,legend=c("observed sampling distribution of ((n-1)s²)/sigma² ","chi(n-1)"),lwd=c(1,2),lty=c(1,2),horiz=T,bty="n")
dev.off()

A<-sum(K<12.72)/length(K) #.147308
B<-sum(link_doublex<12.72)/length(link_doublex) #.275052

B/A # 1.867

C<-sum(K>29.9)/length(K) #.053122
D<-sum(link_doublex>29.9)/length(link_doublex) #.119466

D/C# D/C = 2.249

LOW<-sum(link_k<19)/length(K) # .611646 vs .543365 (in normal case) 
UP<-sum(link_k>19)/length(K) # .388354 vs .456635  (in normal case)

#--------------------------------------------------------
 
mean(link_normal)
mean(link_doublex)
mean(link_mixed)
mean(link_k)
mean(link_skewpos)
mean(link_skewneg)
mean(K)


