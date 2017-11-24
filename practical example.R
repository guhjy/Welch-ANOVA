for (package in c("onewaytests")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

A1 <- c(19.5,31.5,14,27,25,26,9,24,24,36,9,26,24,38,9,22,19,33.5,24.5,27,23.5,21,12,39,15.5,39,10.5,21,14.5,34.5,27.5,32,16,22.5,16.5,35.5,10.5,35.5,27,38.5,24)
A2 <- c(27,19,24,24,24,27,27,22,23,20,17,21,18,19,27,22,27,25,25,21.5,23.5)
#A3 <- c(31,27,39,27,15,19,21,25,25,21,35,35,29,39,35,29,27,29,37,21,23,31,25,23,21,23,29,29,25,17,25)

DV=c(A1,A2,A3)
IV=factor(c(rep(1,length(A1)),rep(2,length(A2)),rep(3,length(A3))))

database<-data.frame(IV = factor(IV), DV) 
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
                     COMPUTING STATISTICS
#------------------------------------------------------------

### add manual computing

# classical F ANOVA
oneway.test(DV ~ IV, data=database, var.equal=TRUE)

# welch's W ANOVA
oneway.test(DV ~ IV, data=database, var.equal=FALSE)

# Brown-Forsythe F* ANOVA
bf.test(DV ~ IV,data=database)#, verbose = F)
