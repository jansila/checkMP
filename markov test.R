setwd("~/Desktop/Models")
require(xlsx)
data<-read.xlsx("import.xlsx",sheetIndex=1,as.data.frame = TRUE, header=TRUE)

ir<-data
ir_diff<-data
ir_diff<-ir_diff[-417,]
ir_diff[,2]<-data[1:416,2]-data[2:417,2]

require(ggplot2)

pdf("data_timeseries.pdf")
par(mfrow=c(1,2))
qplot(x=ir[,1],y=ir[,2],geom="line",xlab = "Date", ylab="Interest rate [%]", main = "Bank of England interest rates")
qplot(x=ir_diff[,1],y=ir_diff[,2],geom="line",xlab = "Date", ylab="Interest rate differences", main = "Bank of England interest rate differences")
dev.off()

pdf("plot_fcast.pdf")
par(mfrow=c(1,2))
qplot(x=seq(from=as.Date('2016-03-17'), by='quarter', length.out=40),y=p2, geom="line", xlab="Date", ylab="Interest rate [%]", main="One instance of the IR forecast")
qplot(x=seq(from=as.Date('2016-03-17'), by='quarter', length.out=40), y=p1, geom="point", xlab="Date", ylab="IR change", main="Prediction of changes")
dev.off()
require(markovchain)

mcFit<-markovchainFit(data=ir_diff[,2])
transition<-mcFit$estimate
transMatrix<-round(mcFit$estimate@transitionMatrix,digits=6)

pdf("MarkovChain_graph.pdf", paper="a4r")
#http://igraph.org/r/doc/plot.common.html
plot(transition, vertex.size2=5, edge.label=NA, edge.arrow.size=0.2, edge.label.cex=0.001,vertex.label.color="floralwhite",vertex.color="darkslateblue")
dev.off()
##### MENTION
steady<-round(steadyStates(transition),digits=6)
##### MENTION
summary(transition)

######
#funkce tvori distribuci podle steady states
myDist<-function(input){
  u<-input
  steadyCum<-cumsum(steady)
  result<-c()
  
  for(i in 2:(length(steady))){
    if( u<=steadyCum[i] & u>steadyCum[i-1]){
     result<-(as.numeric(colnames(steady)[i]))
    }
    if(u<=steadyCum[1]){
     result<-(as.numeric(colnames(steady)[1]))
    }
  }
  return (result)  }

results<-apply(matrix(runif(10000),ncol=20),c(1,2),myDist)  #Here put number of years as ncol
 


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


findSucc<-function(data=transMatrix,from="0"){ 
  #find starting row
  from<-as.character(from)
  colIndices<-which(!data[from,]==0)
  myCumsum <- cumsum(data[from,colIndices])
  u<-runif(1) #draw a random number
  
  if(u>=max(myCumsum)){return(colnames(data)[max(colIndices)])}
  
  else{i<-which.max(myCumsum>u)
  return(colnames(data)[colIndices[i]])
    }
}

forecastMC<-function(currState="0", years=10){
  if(!is.character(currState)){currState<-as.character(currState)}
  
  period<-years*4
  #fcast<-(NaN, start=c(2016,3), frequency=4)
  fcast<-(length=period)
    fcast[1]<-findSucc(data=transMatrix, from=currState)
      for (i in 2:period){
      fcast[i]<-findSucc(data=transMatrix, from=fcast[i-1])}
  #return(ts(fcast, start=c(2016), frequency=4))
  return(fcast)
}


fcastMonteCarlo<-function(years=10){
  #na 0.71 vyber 0, na 0.29 vyber ten s nejvetsi frekvenci
  
  temp<-apply(t(replicate(500, forecastMC(years=years,currState = "0"))),2,as.numeric)
  return(cumsum(colMeans(temp)))
}


fcastIR<-function(series=ir, years=10){
  #Rate<-fcastIR(years=years)
      Rate<-cumsum(forecastMC(years=years))
  Rate[1]<-c(0.5)
  RateRep<-rep(Rate, each=3)
  Rate<-replace(RateRep, RateRep<(-0.05), 0)
       Date<-seq(from=Sys.Date(), by='month', length.out=years*12)
  dfFcast<-data.frame(Date,Rate)
  return(dfFcast)
    }

matpoints(x=IRtest[,1],y=IRtest[,2:101], main="100 simulations of the IR forecast", xlab=NULL)

#Mortgage calculator

mortgage<-function(price=10000,deposit=2000,firstTimer=TRUE, years=10, existCustomer=FALSE)
{
    months<-12*years
    LtV<-(price-deposit)/price
  temp<-fcastIR(years=years)+LtV*1.5+ifelse(LtV>0.9,2,0)+ifelse(firstTimer,0.5,0)+ifelse(existCustomer,-0.1,0)
    start_balance<-c(price-deposit)

  temp$year_rate<-temp$Rate/12/100  
  temp$month<-seq(from=0, by=1, to=months-1)
  #calculate first values
  temp$payment<-start_balance*temp$year_rate*(1+temp$year_rate)^(months-temp$month)/((1+temp$year_rate)^(months-temp$month)-1)
  
  temp$interest<-start_balance*temp$year_rate  
  temp$principal_paid<-temp$payment-temp$interest
  temp$balance<-start_balance-temp$principal_paid
  
  #finish calculation of the balance and payments
  for(i in 2:months){
  temp$payment[i]<-temp$balance[i-1]*temp$year_rate[i]*(1+temp$year_rate[i])^(months-temp$month[i])/((1+temp$year_rate[i])^(months-temp$month[i])-1)
  temp$interest[i]<-temp$balance[i-1]*temp$year_rate[i]  
  temp$principal_paid[i]<-temp$payment[i]-temp$interest[i]
  temp$balance[i]<-temp$balance[i-1]-temp$principal_paid[i]
  }
  # round the values
  temp$balance<-round(temp$balance, digits = 2)
  temp$interest<-round(temp$interest, digits = 4)
  temp$payment<-round(temp$payment, digits = 2)
  temp$principal_paid<-round(temp$principal_paid, digits = 2)
  
  return(temp)
}

descriptive<-function(mortgage=temp){
Tot_payment<-sum(mortgage$payment)
Tot_interest<-sum(mortgage$interest)
Mon_average<-mean(mortgage$payment)
out<-data.frame(Tot_payment,Tot_interest,Mon_average)
colnames(out)<-c("Total payment", "Total interest", "Monthly average payment")
tomelt<-data.frame(mortgage$Date,mortgage$payment,mortgage$principal_paid,mortgage$interest)
test_melt<-melt(tomelt, id.vars = 'mortgage.Date')

p<-ggplot(test_melt, aes(mortgage.Date, value, group=variable, colour=variable)) +geom_line()
p+labs(title="Example of mortgage repayment", x="Date", y="£")
qplot(mortgage$Date,mortgage$balance, main = "Repayment of a loan", ylab="Balance outstanding", xlab="Date")

return(out)
  }


a<-mortgage(years=200)
plot(a$Date, a$Rate, ylim=c(0,max(a$Rate)+1))
abline(h=max(a$Rate))

test<-mortgage()
tomelt<-data.frame(test$Date,test$payment,test$principal_paid,test$interest)
test_melt<-melt(tomelt, id.vars = 'test.Date')

p<-ggplot(test_melt, aes(test.Date, value, group=variable, colour=variable)) +geom_line()
p+labs(title="Example of mortgage repayment", x="Date", y="£", colour="Legend")
p+scale_colour_discrete(name="Legend", labels=c("Total month payment","Payment towards principal","Payment towards interest"))



qplot(test$Date,test$balance, main = "Repayment of a loan", ylab="Balance outstanding", xlab="Date")
# shttp://www.barclays.co.uk/mortgages/mortgage-calculator

