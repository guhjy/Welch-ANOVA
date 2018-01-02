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
  
  
  setwd(dir="C:/Users/Marie/Documents/SIMU/") # destination file
  
  data=readRDS(file = paste0(fname,".rds"))
  return(data)
}


SEff_N_SSR.5<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1719,860))
SEff_N_SSR.55<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1568,863))
SEff_N_SSR.6<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1442,866))
SEff_N_SSR.65<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1335,868))
SEff_N_SSR.7<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1243,871))
SEff_N_SSR.75<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1164,873))
SEff_N_SSR.8<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1094,876))
SEff_N_SSR.85<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(1033,879))
SEff_N_SSR.9<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(978,881))
SEff_N_SSR.95<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(930,884))
SEff_N_SSR1<-read_sample(k=2,distName=c("normal","normal"),m=c(2,3),sds=c(2,8),n=c(886,886))

SEff_Pos_SSR.5<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1719,860))
SEff_Pos_SSR.55<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1568,863))
SEff_Pos_SSR.6<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1442,866))
SEff_Pos_SSR.65<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1335,868))
SEff_Pos_SSR.7<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1243,871))
SEff_Pos_SSR.75<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1164,873))
SEff_Pos_SSR.8<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1094,876))
SEff_Pos_SSR.85<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(1033,879))
SEff_Pos_SSR.9<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(978,881))
SEff_Pos_SSR.95<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(930,884))
SEff_Pos_SSR1<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,3),sds=c(2,8),n=c(886,886))

SEff_KPos_SSR.5<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1719,860))
SEff_KPos_SSR.55<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1568,863))
SEff_KPos_SSR.6<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1442,866))
SEff_KPos_SSR.65<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1335,868))
SEff_KPos_SSR.7<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1243,871))
SEff_KPos_SSR.75<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1164,873))
SEff_KPos_SSR.8<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1094,876))
SEff_KPos_SSR.85<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(1033,879))
SEff_KPos_SSR.9<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(978,881))
SEff_KPos_SSR.95<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(930,884))
SEff_KPos_SSR1<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,3),sds=c(2,8),n=c(886,886))


MEff_N_SSR.5<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(279,140))
MEff_N_SSR.55<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(254,140))
MEff_N_SSR.6<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(234,141))
MEff_N_SSR.65<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(216,141))
MEff_N_SSR.7<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(201,141))
MEff_N_SSR.75<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(189,142))
MEff_N_SSR.8<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(177,142))
MEff_N_SSR.85<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(167,142))
MEff_N_SSR.9<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(159,144))
MEff_N_SSR.95<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(151,144))
MEff_N_SSR1<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(144,144))

MEff_Pos_SSR.5<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(279,140))
MEff_Pos_SSR.55<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(254,140))
MEff_Pos_SSR.6<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(234,141))
MEff_Pos_SSR.65<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(216,141))
MEff_Pos_SSR.7<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(201,141))
MEff_Pos_SSR.75<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(189,142))
MEff_Pos_SSR.8<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(177,142))
MEff_Pos_SSR.85<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(167,142))
MEff_Pos_SSR.9<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(159,144))
MEff_Pos_SSR.95<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(151,144))
MEff_Pos_SSR1<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(144,144))

MEff_KPos_SSR.5<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(279,140))
MEff_KPos_SSR.55<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(254,140))
MEff_KPos_SSR.6<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(234,141))
MEff_KPos_SSR.65<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(216,141))
MEff_KPos_SSR.7<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(201,141))
MEff_KPos_SSR.75<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(189,142))
MEff_KPos_SSR.8<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(177,142))
MEff_KPos_SSR.85<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(167,142))
MEff_KPos_SSR.9<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(159,144))
MEff_KPos_SSR.95<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(151,144))
MEff_KPos_SSR1<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(144,144))

MEff_PosNeg_SSR.5<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(279,140))
MEff_PosNeg_SSR.55<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(254,140))
MEff_PosNeg_SSR.6<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(234,141))
MEff_PosNeg_SSR.65<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(216,141))
MEff_PosNeg_SSR.7<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(201,141))
MEff_PosNeg_SSR.75<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(189,142))
MEff_PosNeg_SSR.8<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(177,142))
MEff_PosNeg_SSR.85<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(167,142))
MEff_PosNeg_SSR.9<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(159,144))
MEff_PosNeg_SSR.95<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(151,144))
MEff_PosNeg_SSR1<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(144,144))

MEff_KNeg_SSR.5<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(279,140))
MEff_KNeg_SSR.55<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(254,140))
MEff_KNeg_SSR.6<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(234,141))
MEff_KNeg_SSR.65<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(216,141))
MEff_KNeg_SSR.7<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(201,141))
MEff_KNeg_SSR.75<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(189,142))
MEff_KNeg_SSR.8<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(177,142))
MEff_KNeg_SSR.85<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(167,142))
MEff_KNeg_SSR.9<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(159,144))
MEff_KNeg_SSR.95<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(151,144))
MEff_KNeg_SSR1<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(144,144))

MEff_N_SSR.5_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(170,85))
MEff_N_SSR.55_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(155,86))
MEff_N_SSR.6_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(142,86))
MEff_N_SSR.65_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(132,86))
MEff_N_SSR.7_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(123,87))
MEff_N_SSR.75_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(115,87))
MEff_N_SSR.8_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(108,87))
MEff_N_SSR.85_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(102,87))
MEff_N_SSR.9_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(97,88))
MEff_N_SSR.95_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(92,88))
MEff_N_SSR1_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,4.5),sds=c(2,8),n=c(88,88))

MEff_Pos_SSR.5_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(170,85))
MEff_Pos_SSR.55_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(155,86))
MEff_Pos_SSR.6_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(142,86))
MEff_Pos_SSR.65_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(132,86))
MEff_Pos_SSR.7_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(123,87))
MEff_Pos_SSR.75_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(115,87))
MEff_Pos_SSR.8_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(108,87))
MEff_Pos_SSR.85_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(102,87))
MEff_Pos_SSR.9_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(97,88))
MEff_Pos_SSR.95_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(92,88))
MEff_Pos_SSR1_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(88,88))

MEff_KPos_SSR.5_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(170,85))
MEff_KPos_SSR.55_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(155,86))
MEff_KPos_SSR.6_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(142,86))
MEff_KPos_SSR.65_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(132,86))
MEff_KPos_SSR.7_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(123,87))
MEff_KPos_SSR.75_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(115,87))
MEff_KPos_SSR.8_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(108,87))
MEff_KPos_SSR.85_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(102,87))
MEff_KPos_SSR.9_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(97,88))
MEff_KPos_SSR.95_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(92,88))
MEff_KPos_SSR1_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,4.5),sds=c(2,8),n=c(88,88))

MEff_PosNeg_SSR.5_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(170,85))
MEff_PosNeg_SSR.55_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(155,86))
MEff_PosNeg_SSR.6_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(142,86))
MEff_PosNeg_SSR.65_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(132,86))
MEff_PosNeg_SSR.7_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(123,87))
MEff_PosNeg_SSR.75_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(115,87))
MEff_PosNeg_SSR.8_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(108,87))
MEff_PosNeg_SSR.85_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(102,87))
MEff_PosNeg_SSR.9_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(97,88))
MEff_PosNeg_SSR.95_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(92,88))
MEff_PosNeg_SSR1_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(88,88))

MEff_Kneg_SSR.5_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(170,85))
MEff_Kneg_SSR.55_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(155,86))
MEff_Kneg_SSR.6_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(142,86))
MEff_Kneg_SSR.65_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(132,86))
MEff_Kneg_SSR.7_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(123,87))
MEff_Kneg_SSR.75_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(115,87))
MEff_Kneg_SSR.8_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(108,87))
MEff_Kneg_SSR.85_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(102,87))
MEff_Kneg_SSR.9_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(97,88))
MEff_Kneg_SSR.95_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(92,88))
MEff_Kneg_SSR1_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,4.5),sds=c(2,8),n=c(88,88))


MEff_N_SSR.5_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(51,51))
MEff_N_SSR.55_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(54,52))
MEff_N_SSR.6_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(56,51))
MEff_N_SSR.65_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(60,51))
MEff_N_SSR.7_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(63,51))
MEff_N_SSR.75_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(67,51))
MEff_N_SSR.8_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(72,51))
MEff_N_SSR.85_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(77,51))
MEff_N_SSR.9_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(83,50))
MEff_N_SSR.95_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(90,50))
MEff_N_SSR1_80pc<-read_sample(k=2,distName=c("normal","normal"),m=c(2,6.255),sds=c(2,8),n=c(99,50))

MEff_Pos_SSR.5_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(51,51))
MEff_Pos_SSR.55_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(54,52))
MEff_Pos_SSR.6_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(56,51))
MEff_Pos_SSR.65_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(60,51))
MEff_Pos_SSR.7_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(63,51))
MEff_Pos_SSR.75_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(67,51))
MEff_Pos_SSR.8_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(72,52))
MEff_Pos_SSR.85_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(77,51))
MEff_Pos_SSR.9_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(83,50))
MEff_Pos_SSR.95_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(90,50))
MEff_Pos_SSR1_80pc<-read_sample(k=2,distName=c("skewpos","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(99,50))

MEff_KPos_SSR.5_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(51,51))
MEff_KPos_SSR.55_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(54,52))
MEff_KPos_SSR.6_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(56,51))
MEff_KPos_SSR.65_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(60,52))
MEff_KPos_SSR.7_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(63,51))
MEff_KPos_SSR.75_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(67,51))
MEff_KPos_SSR.8_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(72,51))
MEff_KPos_SSR.85_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(77,51))
MEff_KPos_SSR.9_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(83,50))
MEff_KPos_SSR.95_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(90,50))
MEff_KPos_SSR1_80pc<-read_sample(k=2,distName=c("chi2","skewpos"),m=c(2,6.255),sds=c(2,8),n=c(99,50))

MEff_PosNeg_SSR.5_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(51,51))
MEff_PosNeg_SSR.55_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(54,52))
MEff_PosNeg_SSR.6_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(56,51))
MEff_PosNeg_SSR.65_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(60,51))
MEff_PosNeg_SSR.7_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(63,51))
MEff_PosNeg_SSR.75_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(67,51))
MEff_PosNeg_SSR.8_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(72,51))
MEff_PosNeg_SSR.85_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(77,51))
MEff_PosNeg_SSR.9_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(83,50))
MEff_PosNeg_SSR.95_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(90,50))
MEff_PosNeg_SSR1_80pc<-read_sample(k=2,distName=c("skewpos","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(99,50))

MEff_Kneg_SSR.5_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(51,51))
MEff_Kneg_SSR.55_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(54,52))
MEff_Kneg_SSR.6_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(56,51))
MEff_Kneg_SSR.65_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(60,51))
MEff_Kneg_SSR.7_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(63,51))
MEff_Kneg_SSR.75_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(67,51))
MEff_Kneg_SSR.8_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(72,51))
MEff_Kneg_SSR.85_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(77,51))
MEff_Kneg_SSR.9_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(83,50))
MEff_Kneg_SSR.95_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(90,50))
MEff_Kneg_SSR1_80pc<-read_sample(k=2,distName=c("chi2","skewneg"),m=c(2,6.255),sds=c(2,8),n=c(99,50))



for (package in c("bda","moments","onewaytests","smoothmest","fGarch", "dplyr","rootSolve")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#### STEP 1: COMPUTING NEEDED SAMPLE SIZE IN ORDER TO ACHIEVE .95 POWER

power_function=function(n1,m2,SSR,expected_power){
  m1<-2
  sd1<-2
  sd2<-8
  n2<-SSR*n1
  
  #Power for Welch's t-test
  df_w <- (sd1^2/n1+sd2^2/n2)^2/(sd1^4/(n1^2*(n1-1))+sd2^4/(n2^2*(n2-1)))
  ncp_w<-(m1-m2)/sqrt((sd1^2/n1)+(sd2^2/n2))
  crit_t <- qt(0.975,df=df_w) #critical t-value
  #function to solve
  (1-(pt((crit_t),df_w,ncp_w)-pt(-(crit_t),df_w,ncp_w)))- expected_power
  
}

SSR=c(.5,.55,.6,.65,.7,.75,.8,.85,.9,.95,1)

# Observed power

power <- function(variable){
    power_results<-round(sum(variable[,1]<.05)/length(variable[,1]),3)
    return(power_results)
  }
  

power_N_Seff <- c(power(SEff_N_SSR.5),power(SEff_N_SSR.55),power(SEff_N_SSR.6),power(SEff_N_SSR.65),power(SEff_N_SSR.7),power(SEff_N_SSR.75),power(SEff_N_SSR.8),power(SEff_N_SSR.85),power(SEff_N_SSR.9),power(SEff_N_SSR.95),power(SEff_N_SSR1))
power_N_Meff <- c(power(MEff_N_SSR.5),power(MEff_N_SSR.55),power(MEff_N_SSR.6),power(MEff_N_SSR.65),power(MEff_N_SSR.7),power(MEff_N_SSR.75),power(MEff_N_SSR.8),power(MEff_N_SSR.85),power(MEff_N_SSR.9),power(MEff_N_SSR.95),power(MEff_N_SSR1))
power_Pos_Seff <- c(power(SEff_Pos_SSR.5),power(SEff_Pos_SSR.55),power(SEff_Pos_SSR.6),power(SEff_Pos_SSR.65),power(SEff_Pos_SSR.7),power(SEff_Pos_SSR.75),power(SEff_Pos_SSR.8),power(SEff_Pos_SSR.85),power(SEff_Pos_SSR.9),power(SEff_Pos_SSR.95),power(SEff_Pos_SSR1))
power_Pos_Meff <- c(power(MEff_Pos_SSR.5),power(MEff_Pos_SSR.55),power(MEff_Pos_SSR.6),power(MEff_Pos_SSR.65),power(MEff_Pos_SSR.7),power(MEff_Pos_SSR.75),power(MEff_Pos_SSR.8),power(MEff_Pos_SSR.85),power(MEff_Pos_SSR.9),power(MEff_Pos_SSR.95),power(MEff_Pos_SSR1))
power_KPos_Seff <- c(power(SEff_KPos_SSR.5),power(SEff_KPos_SSR.55),power(SEff_KPos_SSR.6),power(SEff_KPos_SSR.65),power(SEff_KPos_SSR.7),power(SEff_KPos_SSR.75),power(SEff_KPos_SSR.8),power(SEff_KPos_SSR.85),power(SEff_KPos_SSR.9),power(SEff_KPos_SSR.95),power(SEff_KPos_SSR1))
power_KPos_Meff <- c(power(MEff_KPos_SSR.5),power(MEff_KPos_SSR.55),power(MEff_KPos_SSR.6),power(MEff_KPos_SSR.65),power(MEff_KPos_SSR.7),power(MEff_KPos_SSR.75),power(MEff_KPos_SSR.8),power(MEff_KPos_SSR.85),power(MEff_KPos_SSR.9),power(MEff_KPos_SSR.95),power(MEff_KPos_SSR1))
power_PosNeg_Meff <- c(power(MEff_PosNeg_SSR.5),power(MEff_PosNeg_SSR.55),power(MEff_PosNeg_SSR.6),power(MEff_PosNeg_SSR.65),power(MEff_PosNeg_SSR.7),power(MEff_PosNeg_SSR.75),power(MEff_PosNeg_SSR.8),power(MEff_PosNeg_SSR.85),power(MEff_PosNeg_SSR.9),power(MEff_PosNeg_SSR.95),power(MEff_PosNeg_SSR1))
power_KNeg_Meff <- c(power(MEff_KNeg_SSR.5),power(MEff_KNeg_SSR.55),power(MEff_KNeg_SSR.6),power(MEff_KNeg_SSR.65),power(MEff_KNeg_SSR.7),power(MEff_KNeg_SSR.75),power(MEff_KNeg_SSR.8),power(MEff_KNeg_SSR.85),power(MEff_KNeg_SSR.9),power(MEff_KNeg_SSR.95),power(MEff_KNeg_SSR1))

power_N_Meff_80pc <- c(power(MEff_N_SSR.5_80pc),power(MEff_N_SSR.55_80pc),power(MEff_N_SSR.6_80pc),power(MEff_N_SSR.65_80pc),power(MEff_N_SSR.7_80pc),power(MEff_N_SSR.75_80pc),power(MEff_N_SSR.8_80pc),power(MEff_N_SSR.85_80pc),power(MEff_N_SSR.9_80pc),power(MEff_N_SSR.95_80pc),power(MEff_N_SSR1_80pc))
power_Pos_Meff_80pc <- c(power(MEff_Pos_SSR.5_80pc),power(MEff_Pos_SSR.55_80pc),power(MEff_Pos_SSR.6_80pc),power(MEff_Pos_SSR.65_80pc),power(MEff_Pos_SSR.7_80pc),power(MEff_Pos_SSR.75_80pc),power(MEff_Pos_SSR.8_80pc),power(MEff_Pos_SSR.85_80pc),power(MEff_Pos_SSR.9_80pc),power(MEff_Pos_SSR.95_80pc),power(MEff_Pos_SSR1_80pc))
power_KPos_Meff_80pc <- c(power(MEff_KPos_SSR.5_80pc),power(MEff_KPos_SSR.55_80pc),power(MEff_KPos_SSR.6_80pc),power(MEff_KPos_SSR.65_80pc),power(MEff_KPos_SSR.7_80pc),power(MEff_KPos_SSR.75_80pc),power(MEff_KPos_SSR.8_80pc),power(MEff_KPos_SSR.85_80pc),power(MEff_KPos_SSR.9_80pc),power(MEff_KPos_SSR.95_80pc),power(MEff_KPos_SSR1_80pc))
power_PosNeg_Meff_80pc <- c(power(MEff_PosNeg_SSR.5_80pc),power(MEff_PosNeg_SSR.55_80pc),power(MEff_PosNeg_SSR.6_80pc),power(MEff_PosNeg_SSR.65_80pc),power(MEff_PosNeg_SSR.7_80pc),power(MEff_PosNeg_SSR.75_80pc),power(MEff_PosNeg_SSR.8_80pc),power(MEff_PosNeg_SSR.85_80pc),power(MEff_PosNeg_SSR.9_80pc),power(MEff_PosNeg_SSR.95_80pc),power(MEff_PosNeg_SSR1_80pc))
power_Kneg_Meff_80pc <- c(power(MEff_Kneg_SSR.5_80pc),power(MEff_Kneg_SSR.55_80pc),power(MEff_Kneg_SSR.6_80pc),power(MEff_Kneg_SSR.65_80pc),power(MEff_Kneg_SSR.7_80pc),power(MEff_Kneg_SSR.75_80pc),power(MEff_Kneg_SSR.8_80pc),power(MEff_Kneg_SSR.85_80pc),power(MEff_Kneg_SSR.9_80pc),power(MEff_Kneg_SSR.95_80pc),power(MEff_Kneg_SSR1_80pc))

SSR <- seq(.5,1,.05)

# observed power

setwd("C:/Users/Marie/Document/graphs")

png("SEff_N.png",width=7500,height=1900, res = 300)
par(mar=c(4,4,4,4),xpd=T)
par(xpd=F)
plot(SSR,power_N_Seff,main="small effect size",ylim=c(.70,1),xaxt="n",ylab="observed power",cex.lab=1.5,xlim=c(.5,1),pch="*",cex=1.5,cex.main=1.8)
lines(SSR,power_Pos_Seff,col="pink",pch=19,lty=1,type="o",cex=1.4,lwd=3)
lines(SSR,power_KPos_Seff,col="blue",pch=17,lty=2,type="o")
axis(1,seq(.5,1,.05),SSR)
abline(h=.95,lty=1,col="darkgrey")
abline(h=.855,lty=2,col="darkgrey")
dev.off() 

png("MEff_N.png",width=7500,height=1900, res = 300)
par(mar=c(4,4,6,4),xpd=T)
par(xpd=F)
plot(SSR,power_N_Meff,main="",ylim=c(.85,1),xaxt="n",ylab="observed power",cex.lab=1.5,xlim=c(.5,1),pch="*",cex=1.5,cex.main=1.8)
lines(SSR,power_Pos_Meff,col="pink",pch=19,lty=1,type="o",cex=1.4,lwd=3)
lines(SSR,power_KPos_Meff,col="blue",pch=17,lty=2,type="o")
lines(SSR,power_PosNeg_Meff,col="red",pch=16,lty=2,lwd=3,type="o")
lines(SSR,power_KNeg_Meff,col="darkgreen",pch=16,lty=2,type="o")
axis(1,seq(.5,1,.05),SSR)
abline(h=.95,lty=1,col="darkgrey")
abline(h=.855,lty=2,col="darkgrey")
par(xpd=T)
legend(.55,1.07,legend=c("normal","skewpos","chi2 and skewpos"),lty=c(1,1,1),col=c("black","pink","blue"),cex=1.2,bty="n",horiz=F)
legend(.75,1.05,legend=c("skewpos and skewneg","chi2 and skewneg"),lty=c(1,1),col=c("red","darkgreen"),cex=1.2,bty="n",horiz=F)
dev.off() 


png("MEff_N.png",width=7500,height=1900, res = 300)
par(mar=c(4,4,6,4),xpd=T)
par(xpd=F)
plot(SSR,power_N_Meff_80pc,main="",ylim=c(.90,1))
,xaxt="n",ylab="observed power",cex.lab=1.5,xlim=c(.5,1),pch="*",cex=1.5,cex.main=1.8)
lines(SSR,power_Pos_Meff_80pc,col="pink",pch=19,lty=1,type="o",cex=1.4,lwd=3)
lines(SSR,power_KPos_Meff_80pc,col="blue",pch=17,lty=2,type="o")
lines(SSR,power_PosNeg_Meff_80pc,col="red",pch=16,lty=2,lwd=3,type="o")
lines(SSR,power_Kneg_Meff_80pc,col="darkgreen",pch=16,lty=2,type="o")
axis(1,seq(.5,1,.05),SSR)
abline(h=.95,lty=1,col="darkgrey")
abline(h=.855,lty=2,col="darkgrey")
par(xpd=T)
legend(.55,1.1,legend=c("normal","skewpos","chi2 and skewpos"),lty=c(1,1,1),col=c("black","pink","blue"),cex=1.2,bty="n",horiz=F)
legend(.75,1.1,legend=c("skewpos and skewneg","chi2 and skewneg"),lty=c(1,1),col=c("red","darkgreen"),cex=1.2,bty="n",horiz=F)

dev.off() 


