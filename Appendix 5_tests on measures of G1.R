### skewness in R (package moments)
descr_R=function(variable=list()){
  
  results=matrix(0,14,4)
  rownames(results)=paste0("n=",c(10,15,20,25,30,40,45,50,60,75,80,100,150,200))
  colnames(results)=c("min","max","mean","sd")
  
  for (i in 1:14){
    results[i,1]=round(min(variable[[i]][,1]),2)
    results[i,2]=round(max(variable[[i]][,1]),2)
    results[i,3]=round(mean(variable[[i]][,1]),2)
    results[i,4]=round(sd(variable[[i]][,1]),2)
  }
  return(results)
  
}

### skewness SPSS
descr_SPSS=function(variable=list()){
  
  results=matrix(0,14,4)
  rownames(results)=paste0("n=",c(10,15,20,25,30,40,45,50,60,75,80,100,150,200))
  colnames(results)=c("min","max","mean","sd")
  
  for (i in 1:14){
    results[i,1]=round(min(variable[[i]][,2]),2)
    results[i,2]=round(max(variable[[i]][,2]),2)
    results[i,3]=round(mean(variable[[i]][,2]),2)
    results[i,4]=round(sd(variable[[i]][,2]),2)
  }
  return(results)
  
}

### medcouple
descr_medcouple=function(variable=list()){
  
  results=matrix(0,14,4)
  rownames(results)=paste0("n=",c(10,15,20,25,30,40,45,50,60,75,80,100,150,200))
  colnames(results)=c("min","max","mean","sd")
  
  for (i in 1:14){
    results[i,1]=round(min(variable[[i]][,3]),2)
    results[i,2]=round(max(variable[[i]][,3]),2)
    results[i,3]=round(mean(variable[[i]][,3]),2)
    results[i,4]=round(sd(variable[[i]][,3]),3)
  }
  return(results)
  
}

########################################################################################
############                   IS STANDARISATION RELEVANT?                  ############
########################################################################################

# Measures proposed in R
# conclusion: std_error depends on the distribution underlying the data
# so standarization is NOT relevant

descr_R(variable=list(Normal_n10_sds2,Normal_n15_sds2,Normal_n20_sds2,Normal_n25_sds2,Normal_n30_sds2,Normal_n40_sds2,Normal_n45_sds2,Normal_n50_sds2,Normal_n60_sds2,Normal_n75_sds2,Normal_n80_sds2,Normal_n100_sds2,Normal_n150_sds2,Normal_n200_sds2))
descr_R(variable=list(Unif_n10_sds2,Unif_n15_sds2,Unif_n20_sds2,Unif_n25_sds2,Unif_n30_sds2,Unif_n40_sds2,Unif_n45_sds2,Unif_n50_sds2,Unif_n60_sds2,Unif_n75_sds2,Unif_n80_sds2,Unif_n100_sds2,Unif_n150_sds2,Unif_n200_sds2))
descr_R(variable=list(Doublex_n10_sds2,Doublex_n15_sds2,Doublex_n20_sds2,Doublex_n25_sds2,Doublex_n30_sds2,Doublex_n40_sds2,Doublex_n45_sds2,Doublex_n50_sds2,Doublex_n60_sds2,Doublex_n75_sds2,Doublex_n80_sds2,Doublex_n100_sds2,Doublex_n150_sds2,Doublex_n200_sds2))
descr_R(list(Mixed_n10_sds2,Mixed_n15_sds2,Mixed_n20_sds2,Mixed_n25_sds2,Mixed_n30_sds2,Mixed_n40_sds2,Mixed_n45_sds2,Mixed_n50_sds2,Mixed_n60_sds2,Mixed_n75_sds2,Mixed_n80_sds2,Mixed_n100_sds2,Mixed_n150_sds2,Mixed_n200_sds2))
descr_R(list(Skewpos_n10_sds2,Skewpos_n15_sds2,Skewpos_n20_sds2,Skewpos_n25_sds2,Skewpos_n30_sds2,Skewpos_n40_sds2,Skewpos_n45_sds2,Skewpos_n50_sds2,Skewpos_n60_sds2,Skewpos_n75_sds2,Skewpos_n80_sds2,Skewpos_n100_sds2,Skewpos_n150_sds2,Skewpos_n200_sds2))
descr_R(list(Skewneg_n10_sds2,Skewneg_n15_sds2,Skewneg_n20_sds2,Skewneg_n25_sds2,Skewneg_n30_sds2,Skewneg_n40_sds2,Skewneg_n45_sds2,Skewneg_n50_sds2,Skewneg_n60_sds2,Skewneg_n75_sds2,Skewneg_n80_sds2,Skewneg_n100_sds2,Skewneg_n150_sds2,Skewneg_n200_sds2))
descr_R(list(Chi2_n10_sds2,Chi2_n15_sds2,Chi2_n20_sds2,Chi2_n25_sds2,Chi2_n30_sds2,Chi2_n40_sds2,Chi2_n45_sds2,Chi2_n50_sds2,Chi2_n60_sds2,Chi2_n75_sds2,Chi2_n80_sds2,Chi2_n100_sds2,Chi2_n150_sds2,Chi2_n200_sds2))

descr_R(variable=list(Normal_n10_sds4,Normal_n15_sds4,Normal_n20_sds4,Normal_n25_sds4,Normal_n30_sds4,Normal_n40_sds4,Normal_n45_sds4,Normal_n50_sds4,Normal_n60_sds4,Normal_n75_sds4,Normal_n80_sds4,Normal_n100_sds4,Normal_n150_sds4,Normal_n200_sds4))
descr_R(variable=list(Unif_n10_sds4,Unif_n15_sds4,Unif_n20_sds4,Unif_n25_sds4,Unif_n30_sds4,Unif_n40_sds4,Unif_n45_sds4,Unif_n50_sds4,Unif_n60_sds4,Unif_n75_sds4,Unif_n80_sds4,Unif_n100_sds4,Unif_n150_sds4,Unif_n200_sds4))
descr_R(variable=list(Doublex_n10_sds4,Doublex_n15_sds4,Doublex_n20_sds4,Doublex_n25_sds4,Doublex_n30_sds4,Doublex_n40_sds4,Doublex_n45_sds4,Doublex_n50_sds4,Doublex_n60_sds4,Doublex_n75_sds4,Doublex_n80_sds4,Doublex_n100_sds4,Doublex_n150_sds4,Doublex_n200_sds4))
descr_R(list(Mixed_n10_sds4,Mixed_n15_sds4,Mixed_n20_sds4,Mixed_n25_sds4,Mixed_n30_sds4,Mixed_n40_sds4,Mixed_n45_sds4,Mixed_n50_sds4,Mixed_n60_sds4,Mixed_n75_sds4,Mixed_n80_sds4,Mixed_n100_sds4,Mixed_n150_sds4,Mixed_n200_sds4))
descr_R(list(Skewpos_n10_sds4,Skewpos_n15_sds4,Skewpos_n20_sds4,Skewpos_n25_sds4,Skewpos_n30_sds4,Skewpos_n40_sds4,Skewpos_n45_sds4,Skewpos_n50_sds4,Skewpos_n60_sds4,Skewpos_n75_sds4,Skewpos_n80_sds4,Skewpos_n100_sds4,Skewpos_n150_sds4,Skewpos_n200_sds4))
descr_R(list(Skewneg_n10_sds4,Skewneg_n15_sds4,Skewneg_n20_sds4,Skewneg_n25_sds4,Skewneg_n30_sds4,Skewneg_n40_sds4,Skewneg_n45_sds4,Skewneg_n50_sds4,Skewneg_n60_sds4,Skewneg_n75_sds4,Skewneg_n80_sds4,Skewneg_n100_sds4,Skewneg_n150_sds4,Skewneg_n200_sds4))
descr_R(list(Chi2_n10_sds4,Chi2_n15_sds4,Chi2_n20_sds4,Chi2_n25_sds4,Chi2_n30_sds4,Chi2_n40_sds4,Chi2_n45_sds4,Chi2_n50_sds4,Chi2_n60_sds4,Chi2_n75_sds4,Chi2_n80_sds4,Chi2_n100_sds4,Chi2_n150_sds4,Chi2_n200_sds4))

descr_R(variable=list(Normal_n10_sds8,Normal_n15_sds8,Normal_n20_sds8,Normal_n25_sds8,Normal_n30_sds8,Normal_n40_sds8,Normal_n45_sds8,Normal_n50_sds8,Normal_n60_sds8,Normal_n75_sds8,Normal_n80_sds8,Normal_n100_sds8,Normal_n150_sds8,Normal_n200_sds8))
descr_R(variable=list(Unif_n10_sds8,Unif_n15_sds8,Unif_n20_sds8,Unif_n25_sds8,Unif_n30_sds8,Unif_n40_sds8,Unif_n45_sds8,Unif_n50_sds8,Unif_n60_sds8,Unif_n75_sds8,Unif_n80_sds8,Unif_n100_sds8,Unif_n150_sds8,Unif_n200_sds8))
descr_R(variable=list(Doublex_n10_sds8,Doublex_n15_sds8,Doublex_n20_sds8,Doublex_n25_sds8,Doublex_n30_sds8,Doublex_n40_sds8,Doublex_n45_sds8,Doublex_n50_sds8,Doublex_n60_sds8,Doublex_n75_sds8,Doublex_n80_sds8,Doublex_n100_sds8,Doublex_n150_sds8,Doublex_n200_sds8))
descr_R(list(Mixed_n10_sds8,Mixed_n15_sds8,Mixed_n20_sds8,Mixed_n25_sds8,Mixed_n30_sds8,Mixed_n40_sds8,Mixed_n45_sds8,Mixed_n50_sds8,Mixed_n60_sds8,Mixed_n75_sds8,Mixed_n80_sds8,Mixed_n100_sds8,Mixed_n150_sds8,Mixed_n200_sds8))
descr_R(list(Skewpos_n10_sds8,Skewpos_n15_sds8,Skewpos_n20_sds8,Skewpos_n25_sds8,Skewpos_n30_sds8,Skewpos_n40_sds8,Skewpos_n45_sds8,Skewpos_n50_sds8,Skewpos_n60_sds8,Skewpos_n75_sds8,Skewpos_n80_sds8,Skewpos_n100_sds8,Skewpos_n150_sds8,Skewpos_n200_sds8))
descr_R(list(Skewneg_n10_sds8,Skewneg_n15_sds8,Skewneg_n20_sds8,Skewneg_n25_sds8,Skewneg_n30_sds8,Skewneg_n40_sds8,Skewneg_n45_sds8,Skewneg_n50_sds8,Skewneg_n60_sds8,Skewneg_n75_sds8,Skewneg_n80_sds8,Skewneg_n100_sds8,Skewneg_n150_sds8,Skewneg_n200_sds8))
descr_R(list(Chi2_n10_sds8,Chi2_n15_sds8,Chi2_n20_sds8,Chi2_n25_sds8,Chi2_n30_sds8,Chi2_n40_sds8,Chi2_n45_sds8,Chi2_n50_sds8,Chi2_n60_sds8,Chi2_n75_sds8,Chi2_n80_sds8,Chi2_n100_sds8,Chi2_n150_sds8,Chi2_n200_sds8))

# Measures proposed in SPSS
# conclusion: std_error depends on the distribution underlying the data
# so standarization is NOT relevant

descr_SPSS(variable=list(Normal_n10_sds2,Normal_n15_sds2,Normal_n20_sds2,Normal_n25_sds2,Normal_n30_sds2,Normal_n40_sds2,Normal_n45_sds2,Normal_n50_sds2,Normal_n60_sds2,Normal_n75_sds2,Normal_n80_sds2,Normal_n100_sds2,Normal_n150_sds2,Normal_n200_sds2))
descr_SPSS(variable=list(Unif_n10_sds2,Unif_n15_sds2,Unif_n20_sds2,Unif_n25_sds2,Unif_n30_sds2,Unif_n40_sds2,Unif_n45_sds2,Unif_n50_sds2,Unif_n60_sds2,Unif_n75_sds2,Unif_n80_sds2,Unif_n100_sds2,Unif_n150_sds2,Unif_n200_sds2))
descr_SPSS(variable=list(Doublex_n10_sds2,Doublex_n15_sds2,Doublex_n20_sds2,Doublex_n25_sds2,Doublex_n30_sds2,Doublex_n40_sds2,Doublex_n45_sds2,Doublex_n50_sds2,Doublex_n60_sds2,Doublex_n75_sds2,Doublex_n80_sds2,Doublex_n100_sds2,Doublex_n150_sds2,Doublex_n200_sds2))
descr_SPSS(list(Mixed_n10_sds2,Mixed_n15_sds2,Mixed_n20_sds2,Mixed_n25_sds2,Mixed_n30_sds2,Mixed_n40_sds2,Mixed_n45_sds2,Mixed_n50_sds2,Mixed_n60_sds2,Mixed_n75_sds2,Mixed_n80_sds2,Mixed_n100_sds2,Mixed_n150_sds2,Mixed_n200_sds2))
descr_SPSS(list(Skewpos_n10_sds2,Skewpos_n15_sds2,Skewpos_n20_sds2,Skewpos_n25_sds2,Skewpos_n30_sds2,Skewpos_n40_sds2,Skewpos_n45_sds2,Skewpos_n50_sds2,Skewpos_n60_sds2,Skewpos_n75_sds2,Skewpos_n80_sds2,Skewpos_n100_sds2,Skewpos_n150_sds2,Skewpos_n200_sds2))
descr_SPSS(list(Skewneg_n10_sds2,Skewneg_n15_sds2,Skewneg_n20_sds2,Skewneg_n25_sds2,Skewneg_n30_sds2,Skewneg_n40_sds2,Skewneg_n45_sds2,Skewneg_n50_sds2,Skewneg_n60_sds2,Skewneg_n75_sds2,Skewneg_n80_sds2,Skewneg_n100_sds2,Skewneg_n150_sds2,Skewneg_n200_sds2))
descr_SPSS(list(Chi2_n10_sds2,Chi2_n15_sds2,Chi2_n20_sds2,Chi2_n25_sds2,Chi2_n30_sds2,Chi2_n40_sds2,Chi2_n45_sds2,Chi2_n50_sds2,Chi2_n60_sds2,Chi2_n75_sds2,Chi2_n80_sds2,Chi2_n100_sds2,Chi2_n150_sds2,Chi2_n200_sds2))

descr_SPSS(variable=list(Normal_n10_sds4,Normal_n15_sds4,Normal_n20_sds4,Normal_n25_sds4,Normal_n30_sds4,Normal_n40_sds4,Normal_n45_sds4,Normal_n50_sds4,Normal_n60_sds4,Normal_n75_sds4,Normal_n80_sds4,Normal_n100_sds4,Normal_n150_sds4,Normal_n200_sds4))
descr_SPSS(variable=list(Unif_n10_sds4,Unif_n15_sds4,Unif_n20_sds4,Unif_n25_sds4,Unif_n30_sds4,Unif_n40_sds4,Unif_n45_sds4,Unif_n50_sds4,Unif_n60_sds4,Unif_n75_sds4,Unif_n80_sds4,Unif_n100_sds4,Unif_n150_sds4,Unif_n200_sds4))
descr_SPSS(variable=list(Doublex_n10_sds4,Doublex_n15_sds4,Doublex_n20_sds4,Doublex_n25_sds4,Doublex_n30_sds4,Doublex_n40_sds4,Doublex_n45_sds4,Doublex_n50_sds4,Doublex_n60_sds4,Doublex_n75_sds4,Doublex_n80_sds4,Doublex_n100_sds4,Doublex_n150_sds4,Doublex_n200_sds4))
descr_SPSS(list(Mixed_n10_sds4,Mixed_n15_sds4,Mixed_n20_sds4,Mixed_n25_sds4,Mixed_n30_sds4,Mixed_n40_sds4,Mixed_n45_sds4,Mixed_n50_sds4,Mixed_n60_sds4,Mixed_n75_sds4,Mixed_n80_sds4,Mixed_n100_sds4,Mixed_n150_sds4,Mixed_n200_sds4))
descr_SPSS(list(Skewpos_n10_sds4,Skewpos_n15_sds4,Skewpos_n20_sds4,Skewpos_n25_sds4,Skewpos_n30_sds4,Skewpos_n40_sds4,Skewpos_n45_sds4,Skewpos_n50_sds4,Skewpos_n60_sds4,Skewpos_n75_sds4,Skewpos_n80_sds4,Skewpos_n100_sds4,Skewpos_n150_sds4,Skewpos_n200_sds4))
descr_SPSS(list(Skewneg_n10_sds4,Skewneg_n15_sds4,Skewneg_n20_sds4,Skewneg_n25_sds4,Skewneg_n30_sds4,Skewneg_n40_sds4,Skewneg_n45_sds4,Skewneg_n50_sds4,Skewneg_n60_sds4,Skewneg_n75_sds4,Skewneg_n80_sds4,Skewneg_n100_sds4,Skewneg_n150_sds4,Skewneg_n200_sds4))
descr_SPSS(list(Chi2_n10_sds4,Chi2_n15_sds4,Chi2_n20_sds4,Chi2_n25_sds4,Chi2_n30_sds4,Chi2_n40_sds4,Chi2_n45_sds4,Chi2_n50_sds4,Chi2_n60_sds4,Chi2_n75_sds4,Chi2_n80_sds4,Chi2_n100_sds4,Chi2_n150_sds4,Chi2_n200_sds4))

descr_SPSS(variable=list(Normal_n10_sds8,Normal_n15_sds8,Normal_n20_sds8,Normal_n25_sds8,Normal_n30_sds8,Normal_n40_sds8,Normal_n45_sds8,Normal_n50_sds8,Normal_n60_sds8,Normal_n75_sds8,Normal_n80_sds8,Normal_n100_sds8,Normal_n150_sds8,Normal_n200_sds8))
descr_SPSS(variable=list(Unif_n10_sds8,Unif_n15_sds8,Unif_n20_sds8,Unif_n25_sds8,Unif_n30_sds8,Unif_n40_sds8,Unif_n45_sds8,Unif_n50_sds8,Unif_n60_sds8,Unif_n75_sds8,Unif_n80_sds8,Unif_n100_sds8,Unif_n150_sds8,Unif_n200_sds8))
descr_SPSS(variable=list(Doublex_n10_sds8,Doublex_n15_sds8,Doublex_n20_sds8,Doublex_n25_sds8,Doublex_n30_sds8,Doublex_n40_sds8,Doublex_n45_sds8,Doublex_n50_sds8,Doublex_n60_sds8,Doublex_n75_sds8,Doublex_n80_sds8,Doublex_n100_sds8,Doublex_n150_sds8,Doublex_n200_sds8))
descr_SPSS(list(Mixed_n10_sds8,Mixed_n15_sds8,Mixed_n20_sds8,Mixed_n25_sds8,Mixed_n30_sds8,Mixed_n40_sds8,Mixed_n45_sds8,Mixed_n50_sds8,Mixed_n60_sds8,Mixed_n75_sds8,Mixed_n80_sds8,Mixed_n100_sds8,Mixed_n150_sds8,Mixed_n200_sds8))
descr_SPSS(list(Skewpos_n10_sds8,Skewpos_n15_sds8,Skewpos_n20_sds8,Skewpos_n25_sds8,Skewpos_n30_sds8,Skewpos_n40_sds8,Skewpos_n45_sds8,Skewpos_n50_sds8,Skewpos_n60_sds8,Skewpos_n75_sds8,Skewpos_n80_sds8,Skewpos_n100_sds8,Skewpos_n150_sds8,Skewpos_n200_sds8))
descr_SPSS(list(Skewneg_n10_sds8,Skewneg_n15_sds8,Skewneg_n20_sds8,Skewneg_n25_sds8,Skewneg_n30_sds8,Skewneg_n40_sds8,Skewneg_n45_sds8,Skewneg_n50_sds8,Skewneg_n60_sds8,Skewneg_n75_sds8,Skewneg_n80_sds8,Skewneg_n100_sds8,Skewneg_n150_sds8,Skewneg_n200_sds8))
descr_SPSS(list(Chi2_n10_sds8,Chi2_n15_sds8,Chi2_n20_sds8,Chi2_n25_sds8,Chi2_n30_sds8,Chi2_n40_sds8,Chi2_n45_sds8,Chi2_n50_sds8,Chi2_n60_sds8,Chi2_n75_sds8,Chi2_n80_sds8,Chi2_n100_sds8,Chi2_n150_sds8,Chi2_n200_sds8))


# Medcouple (robust measure of G1)
# conclusion: std_error does NOT depend on the distribution underlying the data
# so standarization is relevant!!!

descr_medcouple(variable=list(Normal_n10_sds2,Normal_n15_sds2,Normal_n20_sds2,Normal_n25_sds2,Normal_n30_sds2,Normal_n40_sds2,Normal_n45_sds2,Normal_n50_sds2,Normal_n60_sds2,Normal_n75_sds2,Normal_n80_sds2,Normal_n100_sds2,Normal_n150_sds2,Normal_n200_sds2))
descr_medcouple(variable=list(Unif_n10_sds2,Unif_n15_sds2,Unif_n20_sds2,Unif_n25_sds2,Unif_n30_sds2,Unif_n40_sds2,Unif_n45_sds2,Unif_n50_sds2,Unif_n60_sds2,Unif_n75_sds2,Unif_n80_sds2,Unif_n100_sds2,Unif_n150_sds2,Unif_n200_sds2))
descr_medcouple(variable=list(Doublex_n10_sds2,Doublex_n15_sds2,Doublex_n20_sds2,Doublex_n25_sds2,Doublex_n30_sds2,Doublex_n40_sds2,Doublex_n45_sds2,Doublex_n50_sds2,Doublex_n60_sds2,Doublex_n75_sds2,Doublex_n80_sds2,Doublex_n100_sds2,Doublex_n150_sds2,Doublex_n200_sds2))
descr_medcouple(list(Mixed_n10_sds2,Mixed_n15_sds2,Mixed_n20_sds2,Mixed_n25_sds2,Mixed_n30_sds2,Mixed_n40_sds2,Mixed_n45_sds2,Mixed_n50_sds2,Mixed_n60_sds2,Mixed_n75_sds2,Mixed_n80_sds2,Mixed_n100_sds2,Mixed_n150_sds2,Mixed_n200_sds2))
descr_medcouple(list(Skewpos_n10_sds2,Skewpos_n15_sds2,Skewpos_n20_sds2,Skewpos_n25_sds2,Skewpos_n30_sds2,Skewpos_n40_sds2,Skewpos_n45_sds2,Skewpos_n50_sds2,Skewpos_n60_sds2,Skewpos_n75_sds2,Skewpos_n80_sds2,Skewpos_n100_sds2,Skewpos_n150_sds2,Skewpos_n200_sds2))
descr_medcouple(list(Skewneg_n10_sds2,Skewneg_n15_sds2,Skewneg_n20_sds2,Skewneg_n25_sds2,Skewneg_n30_sds2,Skewneg_n40_sds2,Skewneg_n45_sds2,Skewneg_n50_sds2,Skewneg_n60_sds2,Skewneg_n75_sds2,Skewneg_n80_sds2,Skewneg_n100_sds2,Skewneg_n150_sds2,Skewneg_n200_sds2))
descr_medcouple(list(Chi2_n10_sds2,Chi2_n15_sds2,Chi2_n20_sds2,Chi2_n25_sds2,Chi2_n30_sds2,Chi2_n40_sds2,Chi2_n45_sds2,Chi2_n50_sds2,Chi2_n60_sds2,Chi2_n75_sds2,Chi2_n80_sds2,Chi2_n100_sds2,Chi2_n150_sds2,Chi2_n200_sds2))

descr_medcouple(variable=list(Normal_n10_sds4,Normal_n15_sds4,Normal_n20_sds4,Normal_n25_sds4,Normal_n30_sds4,Normal_n40_sds4,Normal_n45_sds4,Normal_n50_sds4,Normal_n60_sds4,Normal_n75_sds4,Normal_n80_sds4,Normal_n100_sds4,Normal_n150_sds4,Normal_n200_sds4))
descr_medcouple(variable=list(Unif_n10_sds4,Unif_n15_sds4,Unif_n20_sds4,Unif_n25_sds4,Unif_n30_sds4,Unif_n40_sds4,Unif_n45_sds4,Unif_n50_sds4,Unif_n60_sds4,Unif_n75_sds4,Unif_n80_sds4,Unif_n100_sds4,Unif_n150_sds4,Unif_n200_sds4))
descr_medcouple(variable=list(Doublex_n10_sds4,Doublex_n15_sds4,Doublex_n20_sds4,Doublex_n25_sds4,Doublex_n30_sds4,Doublex_n40_sds4,Doublex_n45_sds4,Doublex_n50_sds4,Doublex_n60_sds4,Doublex_n75_sds4,Doublex_n80_sds4,Doublex_n100_sds4,Doublex_n150_sds4,Doublex_n200_sds4))
descr_medcouple(list(Mixed_n10_sds4,Mixed_n15_sds4,Mixed_n20_sds4,Mixed_n25_sds4,Mixed_n30_sds4,Mixed_n40_sds4,Mixed_n45_sds4,Mixed_n50_sds4,Mixed_n60_sds4,Mixed_n75_sds4,Mixed_n80_sds4,Mixed_n100_sds4,Mixed_n150_sds4,Mixed_n200_sds4))
descr_medcouple(list(Skewpos_n10_sds4,Skewpos_n15_sds4,Skewpos_n20_sds4,Skewpos_n25_sds4,Skewpos_n30_sds4,Skewpos_n40_sds4,Skewpos_n45_sds4,Skewpos_n50_sds4,Skewpos_n60_sds4,Skewpos_n75_sds4,Skewpos_n80_sds4,Skewpos_n100_sds4,Skewpos_n150_sds4,Skewpos_n200_sds4))
descr_medcouple(list(Skewneg_n10_sds4,Skewneg_n15_sds4,Skewneg_n20_sds4,Skewneg_n25_sds4,Skewneg_n30_sds4,Skewneg_n40_sds4,Skewneg_n45_sds4,Skewneg_n50_sds4,Skewneg_n60_sds4,Skewneg_n75_sds4,Skewneg_n80_sds4,Skewneg_n100_sds4,Skewneg_n150_sds4,Skewneg_n200_sds4))
descr_medcouple(list(Chi2_n10_sds4,Chi2_n15_sds4,Chi2_n20_sds4,Chi2_n25_sds4,Chi2_n30_sds4,Chi2_n40_sds4,Chi2_n45_sds4,Chi2_n50_sds4,Chi2_n60_sds4,Chi2_n75_sds4,Chi2_n80_sds4,Chi2_n100_sds4,Chi2_n150_sds4,Chi2_n200_sds4))

descr_medcouple(variable=list(Normal_n10_sds8,Normal_n15_sds8,Normal_n20_sds8,Normal_n25_sds8,Normal_n30_sds8,Normal_n40_sds8,Normal_n45_sds8,Normal_n50_sds8,Normal_n60_sds8,Normal_n75_sds8,Normal_n80_sds8,Normal_n100_sds8,Normal_n150_sds8,Normal_n200_sds8))
descr_medcouple(variable=list(Unif_n10_sds8,Unif_n15_sds8,Unif_n20_sds8,Unif_n25_sds8,Unif_n30_sds8,Unif_n40_sds8,Unif_n45_sds8,Unif_n50_sds8,Unif_n60_sds8,Unif_n75_sds8,Unif_n80_sds8,Unif_n100_sds8,Unif_n150_sds8,Unif_n200_sds8))
descr_medcouple(variable=list(Doublex_n10_sds8,Doublex_n15_sds8,Doublex_n20_sds8,Doublex_n25_sds8,Doublex_n30_sds8,Doublex_n40_sds8,Doublex_n45_sds8,Doublex_n50_sds8,Doublex_n60_sds8,Doublex_n75_sds8,Doublex_n80_sds8,Doublex_n100_sds8,Doublex_n150_sds8,Doublex_n200_sds8))
descr_medcouple(list(Mixed_n10_sds8,Mixed_n15_sds8,Mixed_n20_sds8,Mixed_n25_sds8,Mixed_n30_sds8,Mixed_n40_sds8,Mixed_n45_sds8,Mixed_n50_sds8,Mixed_n60_sds8,Mixed_n75_sds8,Mixed_n80_sds8,Mixed_n100_sds8,Mixed_n150_sds8,Mixed_n200_sds8))
descr_medcouple(list(Skewpos_n10_sds8,Skewpos_n15_sds8,Skewpos_n20_sds8,Skewpos_n25_sds8,Skewpos_n30_sds8,Skewpos_n40_sds8,Skewpos_n45_sds8,Skewpos_n50_sds8,Skewpos_n60_sds8,Skewpos_n75_sds8,Skewpos_n80_sds8,Skewpos_n100_sds8,Skewpos_n150_sds8,Skewpos_n200_sds8))
descr_medcouple(list(Skewneg_n10_sds8,Skewneg_n15_sds8,Skewneg_n20_sds8,Skewneg_n25_sds8,Skewneg_n30_sds8,Skewneg_n40_sds8,Skewneg_n45_sds8,Skewneg_n50_sds8,Skewneg_n60_sds8,Skewneg_n75_sds8,Skewneg_n80_sds8,Skewneg_n100_sds8,Skewneg_n150_sds8,Skewneg_n200_sds8))
descr_medcouple(list(Chi2_n10_sds8,Chi2_n15_sds8,Chi2_n20_sds8,Chi2_n25_sds8,Chi2_n30_sds8,Chi2_n40_sds8,Chi2_n45_sds8,Chi2_n50_sds8,Chi2_n60_sds8,Chi2_n75_sds8,Chi2_n80_sds8,Chi2_n100_sds8,Chi2_n150_sds8,Chi2_n200_sds8))

########################################################################################
########### PROPORTION OF ESTIMATIONS OF MEDCOUPLE THAT FALLS IN THE INTERVAL [-2;+2] ##
########################################################################################

library(moments)

std_skewness=function(variable,n){
  medcouple=variable[,3]
  
  if (n==10){std_error=.265}
  if (n==15){std_error=.273}
  if (n==20){std_error=.219}
  if (n==25){std_error=.218}
  if (n==30){std_error=.187}
  if (n==40){std_error=.166}
  if (n==45){std_error=.165}
  if (n==50){std_error=.150}
  if (n==60){std_error=.138}
  if (n==75){std_error=.128}
  if (n==80){std_error=.121}
  if (n==100){std_error=.109}
  if (n==150){std_error=.090}
  if (n==200){std_error=.078}
  
  std_medcouple=medcouple/std_error
  rep=sum(std_medcouple<2 & std_medcouple> (-2)) /length(std_medcouple)# proportion of std_medcouple falling in the interval [-2;+2]
  plot(density(std_medcouple),xlab=paste0("mean=",round(mean(std_medcouple),2)," and sd=",round(sd(std_medcouple),2)),main=paste0("G1=",round(skewness(std_medcouple),3)," and G2=",round(kurtosis(std_medcouple),3)))
  return(round(rep,2))
}

std_skewness(Normal_n10_sds2,10)
std_skewness(Normal_n15_sds2,15)
std_skewness(Normal_n20_sds2,20)
std_skewness(Normal_n25_sds2,25)
std_skewness(Normal_n30_sds2,30)
std_skewness(Normal_n40_sds2,40)
std_skewness(Normal_n45_sds2,45)
std_skewness(Normal_n50_sds2,50)
std_skewness(Normal_n60_sds2,60)
std_skewness(Normal_n75_sds2,75)
std_skewness(Normal_n80_sds2,80)
std_skewness(Normal_n100_sds2,100)
std_skewness(Normal_n150_sds2,150)
std_skewness(Normal_n200_sds2,200)

std_skewness(Doublex_n10_sds2,10)
std_skewness(Doublex_n15_sds2,15)
std_skewness(Doublex_n20_sds2,20)
std_skewness(Doublex_n25_sds2,25)
std_skewness(Doublex_n30_sds2,30)
std_skewness(Doublex_n40_sds2,40)
std_skewness(Doublex_n45_sds2,45)
std_skewness(Doublex_n50_sds2,50)
std_skewness(Doublex_n60_sds2,60)
std_skewness(Doublex_n75_sds2,75)
std_skewness(Doublex_n80_sds2,80)
std_skewness(Doublex_n100_sds2,100)
std_skewness(Doublex_n150_sds2,150)
std_skewness(Doublex_n200_sds2,200)

std_skewness(Mixed_n10_sds2,10)
std_skewness(Mixed_n15_sds2,15)
std_skewness(Mixed_n20_sds2,20)
std_skewness(Mixed_n25_sds2,25)
std_skewness(Mixed_n30_sds2,30)
std_skewness(Mixed_n40_sds2,40)
std_skewness(Mixed_n45_sds2,45)
std_skewness(Mixed_n50_sds2,50)
std_skewness(Mixed_n60_sds2,60)
std_skewness(Mixed_n75_sds2,75)
std_skewness(Mixed_n80_sds2,80)
std_skewness(Mixed_n100_sds2,100)
std_skewness(Mixed_n150_sds2,150)
std_skewness(Mixed_n200_sds2,200)

# conclusion: everytime distributions are skewed, 95% of the estimations of std_medcouple
# fall in the interval [-2;+2]

std_skewness(Skewpos_n10_sds2,10)
std_skewness(Skewpos_n15_sds2,15)
std_skewness(Skewpos_n20_sds2,20)
std_skewness(Skewpos_n25_sds2,25)
std_skewness(Skewpos_n30_sds2,30)
std_skewness(Skewpos_n40_sds2,40)
std_skewness(Skewpos_n45_sds2,45)
std_skewness(Skewpos_n50_sds2,50)
std_skewness(Skewpos_n60_sds2,60)
std_skewness(Skewpos_n75_sds2,75)
std_skewness(Skewpos_n80_sds2,80)
std_skewness(Skewpos_n100_sds2,100)
std_skewness(Skewpos_n150_sds2,150)
std_skewness(Skewpos_n200_sds2,200)

# conclusion: when distributions are moderately skewed
# still a big number of estimations falls in the interval [-2;+2] --> not interesting!
# even when n=200, power = .70 (still not enough)

std_skewness(Skewneg_n10_sds2,10)
std_skewness(Skewneg_n15_sds2,15)
std_skewness(Skewneg_n20_sds2,20)
std_skewness(Skewneg_n25_sds2,25)
std_skewness(Skewneg_n30_sds2,30)
std_skewness(Skewneg_n40_sds2,40)
std_skewness(Skewneg_n45_sds2,45)
std_skewness(Skewneg_n50_sds2,50)
std_skewness(Skewneg_n60_sds2,60)
std_skewness(Skewneg_n75_sds2,75)
std_skewness(Skewneg_n80_sds2,80)
std_skewness(Skewneg_n100_sds2,100)
std_skewness(Skewneg_n150_sds2,150)
std_skewness(Skewneg_n200_sds2,200)

# conclusion: when distributions are moderately skewed
# a lower number of estimations falls in the interval [-2;+2] --> not interesting!
# even when n=200, power = .70 (still not enough)

std_skewness(Chi2_n10_sds2,10) # power = .20
std_skewness(Chi2_n15_sds2,15) # power = .20
std_skewness(Chi2_n20_sds2,20) # power = .30
std_skewness(Chi2_n25_sds2,25) # power = .32
std_skewness(Chi2_n30_sds2,30) # power = .40
std_skewness(Chi2_n40_sds2,40) # power = .49
std_skewness(Chi2_n45_sds2,45) # power = .51
std_skewness(Chi2_n50_sds2,50) # power = .58
std_skewness(Chi2_n60_sds2,60) # power = .65
std_skewness(Chi2_n75_sds2,75) # power = .71
std_skewness(Chi2_n80_sds2,80) # power = .76
std_skewness(Chi2_n100_sds2,100) # power = .84
std_skewness(Chi2_n150_sds2,150) # power = .94
std_skewness(Chi2_n200_sds2,200) # power = .98

# conclusion: when distributions are highly skewed
# a very smaller number of estimations falls in the interval [-2;+2] --> more interesting but not enough power
# even when n=200, power = .70 (still not enough)

Normal_n10_sds4,Normal_n15_sds4,Normal_n20_sds4,Normal_n25_sds4,Normal_n30_sds4,Normal_n40_sds4,Normal_n45_sds4,Normal_n50_sds4,Normal_n60_sds4,Normal_n75_sds4,Normal_n80_sds4,Normal_n100_sds4,Normal_n150_sds4,Normal_n200_sds4
Unif_n10_sds4,Unif_n15_sds4,Unif_n20_sds4,Unif_n25_sds4,Unif_n30_sds4,Unif_n40_sds4,Unif_n45_sds4,Unif_n50_sds4,Unif_n60_sds4,Unif_n75_sds4,Unif_n80_sds4,Unif_n100_sds4,Unif_n150_sds4,Unif_n200_sds4
Doublex_n10_sds4,Doublex_n15_sds4,Doublex_n20_sds4,Doublex_n25_sds4,Doublex_n30_sds4,Doublex_n40_sds4,Doublex_n45_sds4,Doublex_n50_sds4,Doublex_n60_sds4,Doublex_n75_sds4,Doublex_n80_sds4,Doublex_n100_sds4,Doublex_n150_sds4,Doublex_n200_sds4
Mixed_n10_sds4,Mixed_n15_sds4,Mixed_n20_sds4,Mixed_n25_sds4,Mixed_n30_sds4,Mixed_n40_sds4,Mixed_n45_sds4,Mixed_n50_sds4,Mixed_n60_sds4,Mixed_n75_sds4,Mixed_n80_sds4,Mixed_n100_sds4,Mixed_n150_sds4,Mixed_n200_sds4
Skewpos_n10_sds4,Skewpos_n15_sds4,Skewpos_n20_sds4,Skewpos_n25_sds4,Skewpos_n30_sds4,Skewpos_n40_sds4,Skewpos_n45_sds4,Skewpos_n50_sds4,Skewpos_n60_sds4,Skewpos_n75_sds4,Skewpos_n80_sds4,Skewpos_n100_sds4,Skewpos_n150_sds4,Skewpos_n200_sds4
Skewneg_n10_sds4,Skewneg_n15_sds4,Skewneg_n20_sds4,Skewneg_n25_sds4,Skewneg_n30_sds4,Skewneg_n40_sds4,Skewneg_n45_sds4,Skewneg_n50_sds4,Skewneg_n60_sds4,Skewneg_n75_sds4,Skewneg_n80_sds4,Skewneg_n100_sds4,Skewneg_n150_sds4,Skewneg_n200_sds4
Chi2_n10_sds4,Chi2_n15_sds4,Chi2_n20_sds4,Chi2_n25_sds4,Chi2_n30_sds4,Chi2_n40_sds4,Chi2_n45_sds4,Chi2_n50_sds4,Chi2_n60_sds4,Chi2_n75_sds4,Chi2_n80_sds4,Chi2_n100_sds4,Chi2_n150_sds4,Chi2_n200_sds4

Normal_n10_sds8,Normal_n15_sds8,Normal_n20_sds8,Normal_n25_sds8,Normal_n30_sds8,Normal_n40_sds8,Normal_n45_sds8,Normal_n50_sds8,Normal_n60_sds8,Normal_n75_sds8,Normal_n80_sds8,Normal_n100_sds8,Normal_n150_sds8,Normal_n200_sds8
Unif_n10_sds8,Unif_n15_sds8,Unif_n20_sds8,Unif_n25_sds8,Unif_n30_sds8,Unif_n40_sds8,Unif_n45_sds8,Unif_n50_sds8,Unif_n60_sds8,Unif_n75_sds8,Unif_n80_sds8,Unif_n100_sds8,Unif_n150_sds8,Unif_n200_sds8
Doublex_n10_sds8,Doublex_n15_sds8,Doublex_n20_sds8,Doublex_n25_sds8,Doublex_n30_sds8,Doublex_n40_sds8,Doublex_n45_sds8,Doublex_n50_sds8,Doublex_n60_sds8,Doublex_n75_sds8,Doublex_n80_sds8,Doublex_n100_sds8,Doublex_n150_sds8,Doublex_n200_sds8
Mixed_n10_sds8,Mixed_n15_sds8,Mixed_n20_sds8,Mixed_n25_sds8,Mixed_n30_sds8,Mixed_n40_sds8,Mixed_n45_sds8,Mixed_n50_sds8,Mixed_n60_sds8,Mixed_n75_sds8,Mixed_n80_sds8,Mixed_n100_sds8,Mixed_n150_sds8,Mixed_n200_sds8
Skewpos_n10_sds8,Skewpos_n15_sds8,Skewpos_n20_sds8,Skewpos_n25_sds8,Skewpos_n30_sds8,Skewpos_n40_sds8,Skewpos_n45_sds8,Skewpos_n50_sds8,Skewpos_n60_sds8,Skewpos_n75_sds8,Skewpos_n80_sds8,Skewpos_n100_sds8,Skewpos_n150_sds8,Skewpos_n200_sds8
Skewneg_n10_sds8,Skewneg_n15_sds8,Skewneg_n20_sds8,Skewneg_n25_sds8,Skewneg_n30_sds8,Skewneg_n40_sds8,Skewneg_n45_sds8,Skewneg_n50_sds8,Skewneg_n60_sds8,Skewneg_n75_sds8,Skewneg_n80_sds8,Skewneg_n100_sds8,Skewneg_n150_sds8,Skewneg_n200_sds8
Chi2_n10_sds8,Chi2_n15_sds8,Chi2_n20_sds8,Chi2_n25_sds8,Chi2_n30_sds8,Chi2_n40_sds8,Chi2_n45_sds8,Chi2_n50_sds8,Chi2_n60_sds8,Chi2_n75_sds8,Chi2_n80_sds8,Chi2_n100_sds8,Chi2_n150_sds8,Chi2_n200_sds8


