

checkMP<-function(series=ir_diff[,2]){
  require(markovchain)
  
  transMatrix<-markovchainFit(data=series)$estimate@transitionMatrix
  
  #make the n-2x3 matrix for observations
  subSample<-series[1:(length(series)-(length(series)%%3))]
  
  seqSet1<-matrix(c(subSample[1:(length(subSample)-2)],
                    subSample[2:(length(subSample)-1)],
                    subSample[3:(length(subSample))]
  ),ncol=3) #fill the matrix in reverse order so position 11 is the first obersvation,12 second and 13 third
  #compute row frequencies
  temp<-as.data.frame(seqSet1)
  Nijk<-aggregate(temp, by=temp, length)[1:(ncol(temp)+1)]
  
  seqSet2<-seqSet1[,-3] #make matrix of couples
  temp2<-as.data.frame(seqSet2)
  Nij<-aggregate(temp2, by=temp2, length)[1:(ncol(temp2)+1)] #rowfrequencies included
  
  findNijPjk<-function(Nijk=Nijk, Nij=Nij, trans=transMatrix, row=1){
    i<-Nijk[row,1]
    j<-Nijk[row,2]
    k<-Nijk[row,3]
    
    fromCh<-as.character(j)
    toCh<-as.character(k)
    Pjk<-trans[fromCh,toCh]
    
    m1<-which(Nij[,1]==i)
    m2<-which(Nij[,2]==j)
    m<-c(m1,m2)
    return(Nij[m[anyDuplicated(m)],3]*Pjk)
  }
  test<-c(length=dim(Nijk)[1])
  #compute the test statistic
  for(z in 1:dim(Nijk)[1]){
    foundNijPjk<-findNijPjk(Nijk=Nijk, Nij=Nij, trans=transMatrix, row=z)
    test[z]<-((Nijk[z,4]-foundNijPjk)^2)/foundNijPjk
  }
  result<-sum(test)
  #return value of the test statistic and test at confience level 95% and 99%
  return(list(test_statistic=result,rejection=cbind(result>qchisq(0.95,df=length(series)^3),result>qchisq(0.99,df=length(series)^3))))
}
