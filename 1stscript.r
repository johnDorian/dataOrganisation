###############
###Title: Random code to get the data orgainsed and in the right format.
###Date Created: 16/11/09
###Aim: To get the data into one dataset, and the flow as hourly.
###############

###First section will just load the two data files - quality & flow
	
	#load the flow data
flow<-read.csv("212270_JoorilandFlow.csv",header=T,)
	#fix the dates up
flow$Date<-paste(as.POSIXlt(strptime(flow$Date,"%d/%m/%Y %H:%M")))
	#convert the character dates to posixlt
flow<-data.frame(Date=as.POSIXlt(flow$Date),Total=flow$Total)
	#make sure it worked
head(flow)
#convert the flow into the time series format.
flow2<-timeseries(flow$Date,"%d/%m/%Y %H:%M",flow$Total)
#look at the timeseries formatted data
head(flow2)
#Now aggregate the data into hourly flow.
flow<-hours.agg(flow2,sum,na.rm=F)
#Check out the new data
head(flow)
	#load the quality data
qual<-read.csv("JWQ.csv",header=T)
	#fix the dates up
qual$Date<-paste(strptime(qual$Date,"%d/%m/%Y %H:%M"))
	#make sure it worked
head(qual)
	#problem with first row
qual<-qual[-1,]
	#get rid of the minutes put them in a tempry vector for later.
minutes<-as.numeric(format(as.POSIXlt(qual$Date),"%M"))
	#change the dates to on the hour
qual$Date<-as.POSIXlt(qual$Date)-minutes*60
	#merge the flow on the qual.
t<-merge(qual,flow,by="Date")
#now restore the data frame with the original time
qual <- data.frame(Date=as.POSIXlt(qual$Date)+minutes*60,t[,2:8])
#check the new data frame out
head(qual)
#It seems to be working...


###Now have to figure out how to put the samples together.






###This section will change the time stamp of the quality values to a 15min time scale and than the two can be joined. - In the sense that if the quality sample is taken within that hour of record flow, than that flow is associated with the quality sample.


###Do some summary stats, of the data, get some histograms, boxcox images and the like.

###Output the summary for the latex documents.



####Appendix:
#Code to fix up the fow data from 15mins to hours using timeseries code
timeseries<-
	function(dates,
			 dateformat,
			 data=NULL){
		if(class(data)=="numeric")data.length=length(data)else data.length=length(data[,1])
		if(!is.null(data)&&length(dates)!=data.length)
			stop("Lengths differ between dates (",length(dates),") and data (",data.length, ").")
		dates<-(strptime(dates,dateformat))
		minute<- as.numeric(format(dates,"%M"))
		hour<- as.numeric(format(dates,"%H"))
		day<-as.numeric(format(dates,"%d"))
		week<-as.numeric(format(dates,"%W"))
		month<-as.numeric(format(dates,"%m"))
		year<-as.numeric(format(dates,"%Y"))
		if(is.na(sum(year))){
			stop("Dates must contain atleast three fields")}
		if(is.null(data)){
			warning("NO DATA PROVIDED")
			results<-data.frame(dates,minute,hour,day,week,month,year)
			return(results)
			}
		else{
			results<-data.frame(dates,minute,hour,day,week,month,year,data)
			return(results)
		}
	}
hours.agg<-
	function(data,
			process,
			na.rm=F){
		
		warning("Currently multiples are not possible",call.=F)

		result<-aggregate(list(data=data$data),list(day=data$day,month=data$month,year=data$year,hour=data$hour),process,na.rm=na.rm)
                data <- data.frame(Date=strptime(paste(00,result$hour,result$day,result$month,result$year),"%M %H %d %m %Y"),data=result$data)
                sorted <- data[order(data$Date),]
                final <- data.frame(Date=as.POSIXlt(sorted$Date),Data=sorted$data)
		}
