install.packages("quantmod")
require(quantmod)

marketcap<-read.csv("marketcap.csv", stringsAsFactors = FALSE)
indsum<-aggregate(marketcap$Market.Cap, by=list(Sector=marketcap$Sector), function(x) sum(x,na.rm=TRUE))
#market cap sum by sector
datamerge<-merge(marketcap,indsum,by="Sector")
#create weight by marketcap for each sector
weight<-cbind(datamerge[,1:2],datamerge$Market.Cap/datamerge$x)
names(weight)[3]<-"weight"
weight[,1]<-as.factor(weight[,1])
sector<-levels(weight$Sector)
sector
#Total 11 sectors
for(j in 1:11)
{
symbol<-weight[weight$Sector==sector[j],2] #get symbols for each sector
symbolweight<-weight[weight$Sector==sector[j],3] #wight for each symbol
#quantmod package to get stock data from Yahoo
  for(i in 1:length(symbol))
    {
    tryit <- try(getSymbols(symbol[i], from = "2009-01-01", to = "2017-09-30", src =  "yahoo", adjust =  TRUE))
    if(inherits(tryit, "try-error")){
    i <- i+1
    } else {
    getSymbols(symbol[i], from = "2009-01-01", to = "2017-09-30", src =  "yahoo", adjust =  TRUE)
    return<-dailyReturn(get(symbol[i]))
    weightedreturn<-return*symbolweight[i]
    names(return)<-symbol[i]
    if((i==1)&(j==1)){
      dailyreturndata<-return
      weighteddata<-weightedreturn
    
      } else {  
    dailyreturndata<-merge(dailyreturndata,return)
    weighteddata<-merge(weighteddata,weightedreturn)
    
    }
    }
    }



sectorreturn<-rowSums(weighteddata, na.rm = TRUE)
names(sectorreturn)<-sector[j]
if(j==1) {
tempdata<-cbind(weighteddata,sectorreturn)
sectordata<-tempdata[,ncol(tempdata)]

} else {sectordata<-merge(sectordata,sectorreturn)
}  

}
#transform to time series data
names(sectordata)<-sector

xtsdailyreturn<-as.xts(dailyreturndata)
write.zoo(xtsdailyreturn,"dailyreturndata.csv", sep=",")
xtssector<-as.xts(sectordata)
write.zoo(xtssector,"sectordata.csv", sep=",")

#data that didn't retrieve successfully
different.names <- (!weight[,2] %in% names(dailyreturndata))
weight[different.names,2]

