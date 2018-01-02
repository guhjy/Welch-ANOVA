##### Function to load packages

for (package in c("mrfDepth","bda","moments","onewaytests","smoothmest","fGarch", "moments","dplyr")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# moments = package to compute kurtosis and skewness
# smoothmest = package to generate data from double exponential distribution
# fGarch = package to generate data from normal skewed distribution

##### Function for data generation (for different distributions)

get_sample     <- function(distName,n,              # n = sample size; distName = distribution underlying the data
                           lambda,                  # arguments for double exponential distribution
                           m, sds,                  # arguments for normal, normal skewed or double exponential distribution
                           p1,p2,sd1,sd2,           # arguments for mixed normal
                           min, max,                # arguments for unif distribution
                           df)                      # argument for chi square distribution
{
  
  if (distName=="normal"){ out <- rnorm(n, mean=m, sd=sds)}
  else if (distName=="doublex"){out <- rdoublex(n, mu=m, lambda=lambda/sqrt(2))} # transformation in order that lambda = sd
  else if (distName=="skewpos"){out <- rsnorm(n, mean=m, sd=sds,xi=10)}
  else if (distName=="skewneg"){out <- rsnorm(n, mean=m, sd=sds,xi=-10)}
  else if (distName=="unif"){out <- runif(n, min=min, max=max)}
  else if (distName=="chi2"){out <- rchisq(n, df=df)}
  else if (distName=="chi2A"){out <- rchisq(n, df=df)-31.5}
  else if (distName=="chi2C"){out <- rchisq(n, df=df)-1.5}
  else if (distName=="chi2D"){out <- rchisq(n, df=df)-7.5}
  else if (distName=="mixed"){out <- rmixnorm(n,p=c(p1,p2),mean=rep(m,2),sd=c(sd1,sd2))}
  return (out)
  
}

# example: read_sample(n=1000, distName="normal", m=0, sd=2) will generate 1000 data extracted from a N(0,2)

##### Function to export results in a .txt file with customized name

get_write <- function(object,distName,n,    # object = what we want to export, k=number of groups, n = sample size; distName = distribution underlying the data
                      lambda,                 # arguments for double exponential distribution
                      m, sds,                 # arguments for normal, normal skewed or double exponential distribution
                      p1,p2,sd1,sd2,          # arguments for mixed normal distribution
                      min, max,               # arguments for unif distribution
                      df)                     # degrees of freedom (argument for chi square distribution)
  
{
  
  # compute mean and standard deviation
  mu <- 0
  std <- 0
  distr <-  "G1 and G2 when distr="
  nobs <- " n="
  means <-  " means="
  stdevs <- " sds="
  
    if (distName=="normal"){ 
      mu=m
      std=sds
    }
    else if (distName=="doublex"){
      mu=m
      std=lambda
    }
    else if (distName=="skewpos"){
      mu=m
      std=sds
    }
    else if (distName=="skewneg"){
      mu=m
      std=sds
    }
    else if (distName=="unif"){
      mu=round((min+max)/2,2)
      std=round(sqrt((min-max)^2/12),2)
    }
    else if (distName=="chi2"){
      mu=df
      std=round(sqrt(2*df),2)
    }
    else if (distName=="chi2A"){
      mu=df-31.5
      std=round(sqrt(2*df),2)
    }
    else if (distName=="chi2C"){
      mu=df-1.5
      std=round(sqrt(2*df),2)
    }
    else if (distName=="chi2D"){
      mu=df-7.5
      std=round(sqrt(2*df),2)
    }
     else if (distName=="mixed"){
      mu=m
      std=round(sqrt(p1*sd1^2+p2*sd2^2),2)
    }
    
      distr <- paste0(distr, distName)
      nobs <- paste0(nobs, n)
      means <- paste0(means, mu)
      stdevs <- paste0(stdevs, std)

  fname <-  paste(distr, nobs, means, stdevs, sep=",")
  fname <-  paste0(fname, ".rds")
  saveRDS(object, file = fname)
}

#get_write(5,n=10,distName="normal",m=0,sds=2)

##### Function to generate dataset,realize statistical test and compute (and extract) p-value

get_simu     <- function(nSims=1000000,distName,n,        # k=number of groups, n = sample size; distName = distribution underlying the data
                         lambda,                  # arguments for double exponential distribution
                         m, sds,                # arguments for normal, normal skewed or double exponential distribution
                         p1,p2,sd1,sd2,           # arguments for mixed normal distribution
                         min, max,                # arguments for uniform distribution
                         df)                      # degrees of freedom (for chi square distribution
  
{

  # set up empty container for all estimated parameters
  parameters<-matrix(0,nSims,7) # Three colums to store the p-value.

  colnames(parameters) <- c("skewness_R","skewness_SPSS","skewness_medcouple","kurtosis_R","kurtosis_SPSS","mean","sd") 
  
  # Variables generation (dependant variable and factor)
 
  for (i in 1:nSims){
      y <- get_sample(distName,n,lambda,m,sds,p1,p2,sd1,sd2,min, max,df)
      G1_R <- moment(y, order=3,central=T)/(sd(y)*sqrt((n-1)/n))^3 # skewness in R
      G1_SPSS <- (n^2*moment(y, order=3,central=T))/((n-1)*(n-2)*sd(y)^3) # skewness in SPSS
      G1_MEDCOUPLE <- medcouple(y, do.reflect = NULL)
      G2_R <- (moment(y, order=4,central=T)/(sd(y)*sqrt((n-1)/n))^4) # kurtosis in R
      G2_SPSS <- (n^2*(n+1)*(moment(y, order=4,central=T)))/((n-1)*(n-2)*(n-3)*sd(y)^4)-(3*(n-1)^2)/((n-2)*(n-3))# kurtosis in SPSS
      moy <- mean(y)
      var <- var(y)

      parameters[i,1:7]=c(G1_R,G1_SPSS,G1_MEDCOUPLE,G2_R,G2_SPSS,moy,var) 

       }

  setwd("C:/Users/Marie/Dropbox/ANOVA's Welch/Appendix/Appendix 5_ sampling distribution of G1 and G2")
  get_write(parameters,distName,n,lambda,m,sds,p1,p2,sd1,sd2,min,max,df)
  

}

##################### sds=2 #####################
 
### normal with no effect
get_simu(distName="normal",n=10,m=0,sds=2)
get_simu(distName="normal",n=15,m=0,sds=2)
get_simu(distName="normal",n=20,m=0,sds=2)
get_simu(distName="normal",n=25,m=0,sds=2)
get_simu(distName="normal",n=30,m=0,sds=2)
get_simu(distName="normal",n=40,m=0,sds=2)
get_simu(distName="normal",n=45,m=0,sds=2)
get_simu(distName="normal",n=50,m=0,sds=2)
get_simu(distName="normal",n=60,m=0,sds=2)
get_simu(distName="normal",n=75,m=0,sds=2)
get_simu(distName="normal",n=80,m=0,sds=2)
get_simu(distName="normal",n=100,m=0,sds=2)
get_simu(distName="normal",n=150,m=0,sds=2)
get_simu(distName="normal",n=200,m=0,sds=2)


### double exponential 
get_simu(distName="doublex",n=10,m=0,lambda=2)
get_simu(distName="doublex",n=15,m=0,lambda=2)
get_simu(distName="doublex",n=20,m=0,lambda=2)
get_simu(distName="doublex",n=25,m=0,lambda=2)
get_simu(distName="doublex",n=30,m=0,lambda=2)
get_simu(distName="doublex",n=40,m=0,lambda=2)
get_simu(distName="doublex",n=45,m=0,lambda=2)
get_simu(distName="doublex",n=50,m=0,lambda=2)
get_simu(distName="doublex",n=60,m=0,lambda=2)
get_simu(distName="doublex",n=75,m=0,lambda=2)
get_simu(distName="doublex",n=80,m=0,lambda=2)
get_simu(distName="doublex",n=100,m=0,lambda=2)
get_simu(distName="doublex",n=150,m=0,lambda=2)
get_simu(distName="doublex",n=200,m=0,lambda=2)


Unif_n10_sds2=get_simu(nSims=1,distName="unif",n=10,min=-3.465,max=3.465)
Unif_n15_sds2=get_simu(distName="unif",n=15,min=-3.465,max=3.465)
Unif_n20_sds2=get_simu(distName="unif",n=20,min=-3.465,max=3.465)
Unif_n25_sds2=get_simu(distName="unif",n=25,min=-3.465,max=3.465)
Unif_n30_sds2=get_simu(distName="unif",n=30,min=-3.465,max=3.465)
Unif_n40_sds2=get_simu(distName="unif",n=40,min=-3.465,max=3.465)
Unif_n45_sds2=get_simu(distName="unif",n=45,min=-3.465,max=3.465)
Unif_n50_sds2=get_simu(distName="unif",n=50,min=-3.465,max=3.465)
Unif_n60_sds2=get_simu(distName="unif",n=60,min=-3.465,max=3.465)
Unif_n75_sds2=get_simu(distName="unif",n=75,min=-3.465,max=3.465)
Unif_n80_sds2=get_simu(distName="unif",n=80,min=-3.465,max=3.465)
Unif_n100_sds2=get_simu(distName="unif",n=100,min=-3.465,max=3.465)
Unif_n150_sds2=get_simu(distName="unif",n=150,min=-3.465,max=3.465)
Unif_n200_sds2=get_simu(distName="unif",n=200,min=-3.465,max=3.465)

# mixed normal with no effect
get_simu(distName="mixed",n=10,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=15,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=20,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=25,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=30,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=40,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=45,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=50,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=60,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=75,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=80,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=100,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=150,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)
get_simu(distName="mixed",n=200,m=0,p1=.1,p2=.9,sd1=5.06,sd2=1.265)

### normal skewed with positive skewness
get_simu(distName="skewpos",n=10,m=0,sds=2)
get_simu(distName="skewpos",n=15,m=0,sds=2)
get_simu(distName="skewpos",n=20,m=0,sds=2)
get_simu(distName="skewpos",n=25,m=0,sds=2)
get_simu(distName="skewpos",n=30,m=0,sds=2)
get_simu(distName="skewpos",n=40,m=0,sds=2)
get_simu(distName="skewpos",n=45,m=0,sds=2)
get_simu(distName="skewpos",n=50,m=0,sds=2)
get_simu(distName="skewpos",n=60,m=0,sds=2)
get_simu(distName="skewpos",n=75,m=0,sds=2)
get_simu(distName="skewpos",n=80,m=0,sds=2)
get_simu(distName="skewpos",n=100,m=0,sds=2)
get_simu(distName="skewpos",n=150,m=0,sds=2)
get_simu(distName="skewpos",n=200,m=0,sds=2)

### normal skewed with negative skewness
get_simu(distName="skewneg",n=10,m=0,sds=2)
get_simu(distName="skewneg",n=15,m=0,sds=2)
get_simu(distName="skewneg",n=20,m=0,sds=2)
get_simu(distName="skewneg",n=25,m=0,sds=2)
get_simu(distName="skewneg",n=30,m=0,sds=2)
get_simu(distName="skewneg",n=40,m=0,sds=2)
get_simu(distName="skewneg",n=45,m=0,sds=2)
get_simu(distName="skewneg",n=50,m=0,sds=2)
get_simu(distName="skewneg",n=60,m=0,sds=2)
get_simu(distName="skewneg",n=75,m=0,sds=2)
get_simu(distName="skewneg",n=80,m=0,sds=2)
get_simu(distName="skewneg",n=100,m=0,sds=2)
get_simu(distName="skewneg",n=150,m=0,sds=2)
get_simu(distName="skewneg",n=200,m=0,sds=2)

### chi²
get_simu(distName="chi2C",n=10,df=2)
get_simu(distName="chi2C",n=15,df=2)
get_simu(distName="chi2C",n=20,df=2)
get_simu(distName="chi2C",n=25,df=2)
get_simu(distName="chi2C",n=30,df=2)
get_simu(distName="chi2C",n=40,df=2)
get_simu(distName="chi2C",n=45,df=2)
get_simu(distName="chi2C",n=50,df=2)
get_simu(distName="chi2C",n=60,df=2)
get_simu(distName="chi2C",n=75,df=2)
get_simu(distName="chi2C",n=80,df=2)
get_simu(distName="chi2C",n=100,df=2)
get_simu(distName="chi2C",n=150,df=2)
get_simu(distName="chi2C",n=200,df=2)

##################### sds=4 #####################

### normal with no effect
get_simu(distName="normal",n=10,m=0,sds=4)
get_simu(distName="normal",n=15,m=0,sds=4)
get_simu(distName="normal",n=20,m=0,sds=4)
get_simu(distName="normal",n=25,m=0,sds=4)
get_simu(distName="normal",n=30,m=0,sds=4)
get_simu(distName="normal",n=40,m=0,sds=4)
get_simu(distName="normal",n=45,m=0,sds=4)
get_simu(distName="normal",n=50,m=0,sds=4)
get_simu(distName="normal",n=60,m=0,sds=4)
get_simu(distName="normal",n=75,m=0,sds=4)
get_simu(distName="normal",n=80,m=0,sds=4)
get_simu(distName="normal",n=100,m=0,sds=4)
get_simu(distName="normal",n=150,m=0,sds=4)
get_simu(distName="normal",n=200,m=0,sds=4)

### double exponential 
get_simu(distName="doublex",n=10,m=0,lambda=4)
get_simu(distName="doublex",n=15,m=0,lambda=4)
get_simu(distName="doublex",n=20,m=0,lambda=4)
get_simu(distName="doublex",n=25,m=0,lambda=4)
get_simu(distName="doublex",n=30,m=0,lambda=4)
get_simu(distName="doublex",n=40,m=0,lambda=4)
get_simu(distName="doublex",n=45,m=0,lambda=4)
get_simu(distName="doublex",n=50,m=0,lambda=4)
get_simu(distName="doublex",n=60,m=0,lambda=4)
get_simu(distName="doublex",n=75,m=0,lambda=4)
get_simu(distName="doublex",n=80,m=0,lambda=4)
get_simu(distName="doublex",n=100,m=0,lambda=4)
get_simu(distName="doublex",n=150,m=0,lambda=4)
get_simu(distName="doublex",n=200,m=0,lambda=4)

# mixed normal with no effect
get_simu(distName="mixed",n=10,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=15,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=20,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=25,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=30,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=40,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=45,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=50,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=60,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=75,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=80,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=100,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=150,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)
get_simu(distName="mixed",n=200,m=0,p1=.1,p2=.9,sd1=10.119,sd2=2.53)

### normal skewed with positive skewness
get_simu(distName="skewpos",n=10,m=0,sds=4)
get_simu(distName="skewpos",n=15,m=0,sds=4)
get_simu(distName="skewpos",n=20,m=0,sds=4)
get_simu(distName="skewpos",n=25,m=0,sds=4)
get_simu(distName="skewpos",n=30,m=0,sds=4)
get_simu(distName="skewpos",n=40,m=0,sds=4)
get_simu(distName="skewpos",n=45,m=0,sds=4)
get_simu(distName="skewpos",n=50,m=0,sds=4)
get_simu(distName="skewpos",n=60,m=0,sds=4)
get_simu(distName="skewpos",n=75,m=0,sds=4)
get_simu(distName="skewpos",n=80,m=0,sds=4)
get_simu(distName="skewpos",n=100,m=0,sds=4)
get_simu(distName="skewpos",n=150,m=0,sds=4)
get_simu(distName="skewpos",n=200,m=0,sds=4)

### normal skewed with negative skewness
get_simu(distName="skewneg",n=10,m=0,sds=4)
get_simu(distName="skewneg",n=15,m=0,sds=4)
get_simu(distName="skewneg",n=20,m=0,sds=4)
get_simu(distName="skewneg",n=25,m=0,sds=4)
get_simu(distName="skewneg",n=30,m=0,sds=4)
get_simu(distName="skewneg",n=40,m=0,sds=4)
get_simu(distName="skewneg",n=45,m=0,sds=4)
get_simu(distName="skewneg",n=50,m=0,sds=4)
get_simu(distName="skewneg",n=60,m=0,sds=4)
get_simu(distName="skewneg",n=75,m=0,sds=4)
get_simu(distName="skewneg",n=80,m=0,sds=4)
get_simu(distName="skewneg",n=100,m=0,sds=4)
get_simu(distName="skewneg",n=150,m=0,sds=4)
get_simu(distName="skewneg",n=200,m=0,sds=4)

### chi²

get_simu(distName="chi2D",n=10,df=8)
get_simu(distName="chi2D",n=15,df=8)
get_simu(distName="chi2D",n=20,df=8)
get_simu(distName="chi2D",n=25,df=8)
get_simu(distName="chi2D",n=30,df=8)
get_simu(distName="chi2D",n=40,df=8)
get_simu(distName="chi2D",n=45,df=8)
get_simu(distName="chi2D",n=50,df=8)
get_simu(distName="chi2D",n=60,df=8)
get_simu(distName="chi2D",n=75,df=8)
get_simu(distName="chi2D",n=80,df=8)
get_simu(distName="chi2D",n=100,df=8)
get_simu(distName="chi2D",n=150,df=8)
get_simu(distName="chi2D",n=200,df=8)

##################### sds=8 #####################

### normal with no effect
get_simu(distName="normal",n=10,m=0,sds=8)
get_simu(distName="normal",n=15,m=0,sds=8)
get_simu(distName="normal",n=20,m=0,sds=8)
get_simu(distName="normal",n=25,m=0,sds=8)
get_simu(distName="normal",n=30,m=0,sds=8)
get_simu(distName="normal",n=40,m=0,sds=8)
get_simu(distName="normal",n=45,m=0,sds=8)
get_simu(distName="normal",n=50,m=0,sds=8)
get_simu(distName="normal",n=60,m=0,sds=8)
get_simu(distName="normal",n=75,m=0,sds=8)
get_simu(distName="normal",n=80,m=0,sds=8)
get_simu(distName="normal",n=100,m=0,sds=8)
get_simu(distName="normal",n=150,m=0,sds=8)
get_simu(distName="normal",n=200,m=0,sds=8)


### double exponential 
get_simu(distName="doublex",n=10,m=0,lambda=8)
get_simu(distName="doublex",n=15,m=0,lambda=8)
get_simu(distName="doublex",n=20,m=0,lambda=8)
get_simu(distName="doublex",n=25,m=0,lambda=8)
get_simu(distName="doublex",n=30,m=0,lambda=8)
get_simu(distName="doublex",n=40,m=0,lambda=8)
get_simu(distName="doublex",n=45,m=0,lambda=8)
get_simu(distName="doublex",n=50,m=0,lambda=8)
get_simu(distName="doublex",n=60,m=0,lambda=8)
get_simu(distName="doublex",n=75,m=0,lambda=8)
get_simu(distName="doublex",n=80,m=0,lambda=8)
get_simu(distName="doublex",n=100,m=0,lambda=8)
get_simu(distName="doublex",n=150,m=0,lambda=8)
get_simu(distName="doublex",n=200,m=0,lambda=8)

# mixed normal with no effect
get_simu(distName="mixed",n=10,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=15,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=20,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=25,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=30,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=40,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=45,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=50,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=60,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=75,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=80,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=100,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=150,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)
get_simu(distName="mixed",n=200,m=0,p1=.1,p2=.9,sd1=20.239,sd2=5.06)

### normal skewed with positive skewness
get_simu(distName="skewpos",n=10,m=0,sds=8)
get_simu(distName="skewpos",n=15,m=0,sds=8)
get_simu(distName="skewpos",n=20,m=0,sds=8)
get_simu(distName="skewpos",n=25,m=0,sds=8)
get_simu(distName="skewpos",n=30,m=0,sds=8)
get_simu(distName="skewpos",n=40,m=0,sds=8)
get_simu(distName="skewpos",n=45,m=0,sds=8)
get_simu(distName="skewpos",n=50,m=0,sds=8)
get_simu(distName="skewpos",n=60,m=0,sds=8)
get_simu(distName="skewpos",n=75,m=0,sds=8)
get_simu(distName="skewpos",n=80,m=0,sds=8)
get_simu(distName="skewpos",n=100,m=0,sds=8)
get_simu(distName="skewpos",n=150,m=0,sds=8)
get_simu(distName="skewpos",n=200,m=0,sds=8)

### normal skewed with negative skewness
get_simu(distName="skewneg",n=10,m=0,sds=8)
get_simu(distName="skewneg",n=15,m=0,sds=8)
get_simu(distName="skewneg",n=20,m=0,sds=8)
get_simu(distName="skewneg",n=25,m=0,sds=8)
get_simu(distName="skewneg",n=30,m=0,sds=8)
get_simu(distName="skewneg",n=40,m=0,sds=8)
get_simu(distName="skewneg",n=45,m=0,sds=8)
get_simu(distName="skewneg",n=50,m=0,sds=8)
get_simu(distName="skewneg",n=60,m=0,sds=8)
get_simu(distName="skewneg",n=75,m=0,sds=8)
get_simu(distName="skewneg",n=80,m=0,sds=8)
get_simu(distName="skewneg",n=100,m=0,sds=8)
get_simu(distName="skewneg",n=150,m=0,sds=8)
get_simu(distName="skewneg",n=200,m=0,sds=8)

### chi²
get_simu(distName="chi2A",n=10,df=32)
get_simu(distName="chi2A",n=15,df=32)
get_simu(distName="chi2A",n=20,df=32)
get_simu(distName="chi2A",n=25,df=32)
get_simu(distName="chi2A",n=30,df=32)
get_simu(distName="chi2A",n=40,df=32)
get_simu(distName="chi2A",n=45,df=32)
get_simu(distName="chi2A",n=50,df=32)
get_simu(distName="chi2A",n=60,df=32)
get_simu(distName="chi2A",n=75,df=32)
get_simu(distName="chi2A",n=80,df=32)
get_simu(distName="chi2A",n=100,df=32)
get_simu(distName="chi2A",n=150,df=32)
get_simu(distName="chi2A",n=200,df=32)




