for (package in c("onewaytests")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

A1 <- c(19.5,31.5,14,27,25,26,9,24,24,36,9,26,24,38,9,22,19,33.5,24.5,27,23.5,21,12,39,15.5,39,10.5,21,14.5,34.5,27.5,32,16,22.5,16.5,35.5,10.5,35.5,27,38.5,24)
A2 <- c(27,19,24,24,24,27,27,22,23,20,17,21,18,19,27,22,27,25,25,21.5,23.5)
A3 <- c(31,27,39,27,15,19,21,25,25,21,35,35,29,39,35,29,27,29,37,21,23,31,25,23,21,23,29,29,25,17,25)

DV=c(A1,A2,A3)
IV=factor(c(rep(1,length(A1)),rep(2,length(A2)),rep(3,length(A3))))

database<-data.frame(IV = factor(IV), DV) 

write.table(database,"lalala.txt",sep=";",dec=",")
View(database)

##### Database Summary (See table 1)
k<-length(table(IV))
N=length(DV)
mu<-mean(DV)
variance=var(DV)

groups_param<-matrix(rep(0,3*(k+1)),3,k+1)
colnames(groups_param)=c(paste0("group",1:k),"all data")
rownames(groups_param)=c("n","mu","s^2")
groups_param[1,]<-c(tapply(DV,IV,length),N)
groups_param[2,]<-c(tapply(DV,IV,mean),round(mu,2))
groups_param[3,]<-c(tapply(DV,IV,var),round(variance,2))
groups_param

#------------------------------------------------------------
                   Performing a Shapiro-Wilk test
#------------------------------------------------------------

shapiro.test(database$DV[database$IV==1]) # W=0.95365; p = .09 --> NRH0
shapiro.test(database$DV[database$IV==2]) # W=0.93191; p = .1503 --> NRH0
shapiro.test(database$DV[database$IV==3]) # W=0.96785; p = .462 --> NRH0

# Conclusion: the test does not reject the normality assumption
#------------------------------------------------------------
                     Performing a W-test
#------------------------------------------------------------

oneway.test(DV ~ IV, data=database, var.equal=FALSE)

# APA norms: F(2,59.32)=4.61; p = .014 --> RH0 

#------------------------------------------------------------
                     COMPUTING STATISTICS
#------------------------------------------------------------

############## required statistiques

k<-length(table(IV)) # number of groups
N=length(DV) # total sample size
n = groups_param[1,1:3] # groups sample sizes
means_k = groups_param[2,1:3] # groups means
var_k = groups_param[3,1:3] # groups variances

##################### Computing F-test ######################

global_mean<-sum(n*means_k)/sum(n)

F_stat <- (sum(n*(means-mu)^2)/(k-1))/(sum((n-1)*var_k)/(N-k))
F_df_num <- k-1
F_df_denom <- N-k

F_stat
F_df_num
F_df_denom

#################### Computing F*-test ######################

weighted_var <- (1-n/N)*var_k

BF_stat <- (sum(n*(means-mu)^2))/(sum((1-n/N)*var_k))
BF_df_num <- k-1
BF_df_denom <- 1/(sum(((weighted_var/sum(weighted_var))^2)/(n-1)))

BF_stat
BF_df_num
BF_df_denom

#################### Computing W-test ######################

w_k <-n/var_k
w <- sum(w_k)
welch_mu <- sum(w_k*means_k)/w

W_stat <- (sum(w_k*(means_k-welch_mu)^2))/(k-1)/(1+(2*(k-2)/(k^2-1))*sum((1/(n-1))*(1-w_k/w)^2))
W_df_num <- k-1
W_df_denom <- (k^2-1)/(3*sum((1-w_k/w)^2/(n-1)))

W_stat
W_df_num
W_df_denom
