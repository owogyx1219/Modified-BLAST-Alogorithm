install.packages("seqinr")
library(seqinr)
mel_data <- read.fasta(file = "C:/data/length100.fa",as.string = F)
psd_data <- read.fasta(file = "C:/data/length100b.fa",as.string = F)
attach(mel_data)
attach(psd_data)
## mel & psd are strings
circle_dna = function(S2,S3,gap){
  #attach(S2)
  #attach(S3)
  l_m=length(S2$mel)
  l_p=length(S3$psd)
  
  cal_score = function(x,y){
    if(x=="a" & y=="a") return(91)
    if(x=="c" & y=="c") return(100)
    if(x=="g" & y=="g") return(100)
    if(x=="t" & y=="t") return(91)
    
    if((x=="a" & y=="c") | (x=="c" & y=="a")) return(-114)
    if((x=="a" & y=="g") | (x=="g" & y=="a")) return(-31)
    if((x=="a" & y=="t") | (x=="t" & y=="a")) return(-123)
    
    if((x=="c" & y=="g") | (x=="g" & y=="c")) return(-125)
    if((x=="c" & y=="t") | (x=="t" & y=="c")) return(-31)
    
    if((x=="g" & y=="t") | (x=="t" & y=="g")) return(-114)
  }
  
  ## create a empty score matrix
  score = matrix(0, nrow = l_p, ncol = l_m)
  score_array=matrix(0,nrow = 1, ncol = l_m)
##add a loop here######################################################
  for(start in 1:l_m){
  ## first line and first column
    for(i in 1:l_m){
      if(i==1) score[1,i] = cal_score(S2$mel[start],S3$psd[1])
      else{
        score[1,i] = score[1,i-1]+gap
      }
    }
    for(j in 2:l_p){
      score[j,1]=score[j-1,1]+gap
    }
    
  ## the rest of the matrix
    for (i in 2:l_m) {
      for (j in 2:l_p) {
        score[j,i]=max((score[j-1,i-1]+cal_score(S2$mel[(i+start-2)%%l_m+1],S3$psd[j])), 
                       score[j-1,i]+gap, score[j,i-1]+gap)
      }
    }
    score_array[start]=score[l_p,l_m]
}
  
  best=which.max(score_array)
  
################################################################
  #update the best score matrix
################################################################
  
  for(i in 1:l_m){
    if(i==1) score[1,i] = cal_score(S2$mel[best],S3$psd[1])
    else{
      score[1,i] = score[1,i-1]+gap
    }
  }
  for(j in 2:l_p){
    score[j,1]=score[j-1,1]+gap
  }
  
  ## the rest of the matrix
  for (i in 2:l_m) {
    for (j in 2:l_p) {
      score[j,i]=max((score[j-1,i-1]+cal_score(S2$mel[(i+best-2)%%l_m+1],S3$psd[j])), 
                     score[j-1,i]+gap, score[j,i-1]+gap)
    }
  }
  
  
################################################################
  #Íù»ØÕÒpath
################################################################
  
  ## find max's index in a matrix
  #coord = which(score == max(score), arr.ind = TRUE)
  
  match = matrix(, nrow = 2, ncol = 1000)
  temp_x=l_p
  temp_y=l_m
  for (i in 1:1000){
    if (temp_x<=1 | temp_y<=1)
    { if (temp_x==1 & temp_y==1) 
      {match[1,i]=mel[1] 
      match[2,i]=psd[1]}
      if (temp_x==1 & temp_y==2) 
      {match[1,i]=NA
       match[2,i]=psd[1]}
      if (temp_x==2 & temp_y==1) 
      {match[1,i]=mel[1]
       match[2,i]=NA}
    break
    }
    
    if(score[temp_x-1,temp_y-1]>score[temp_x-1,temp_y] & score[temp_x-1,temp_y-1]>score[temp_x,temp_y-1])
    {match[1,i]=S2$mel[temp_y] 
    match[2,i]=S3$psd[temp_x]
    temp_x=temp_x-1
    temp_y=temp_y-1}
    
    else if(score[temp_x,temp_y-1]>score[temp_x-1,temp_y-1] & score[temp_x,temp_y-1]>score[temp_x-1,temp_y])
    {match[1,i]=S2$mel[temp_y] 
    match[2,i]=NA
    temp_x=temp_x
    temp_y=temp_y-1}
    
    else if(score[temp_x-1,temp_y]>score[temp_x-1,temp_y-1] & score[temp_x-1,temp_y]>score[temp_x,temp_y-1])
    {match[1,i]=NA  
    match[2,i]=S3$psd[temp_x]
    temp_x=temp_x-1
    temp_y=temp_y}
  }
  
  count=0
  for(i in 1:1000){
    if(is.na(match[1,i]) & is.na(match[2,i]))
      count=count+1
  } 
  
  a=match[1,1000-count:1000]
  b=match[2,1000-count:1000]
  a[is.na(a)] <- "-" 
  b[is.na(b)] <- "-"
  score=score[l_p,l_m]
  result=rbind(a,b)
  #print("The optimal alignment between given sequences has score:")
  #print(score[l_p,l_m])
  #print("The corresponding alignment pattern is:")
  #########################################################
  # write score array and best score matrix to file 
  #########################################################
  write.csv(rbind(a,b),"C:/data/result/sequence_alignment.csv")
  write.csv(score_array,"C:/data/result/score_array.csv")
  write.csv(score,"C:/data/result/best_score_matrix.csv")
  write.table(best,"C:/data/result/best_score_index.txt")
  return(rbind(a,b))
}

result=circle_dna(mel_data,psd_data,-100)



