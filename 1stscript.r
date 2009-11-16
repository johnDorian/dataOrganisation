###############
###Title: Random code to get the data orgainsed and in the right format.
###Date Created: 16/11/09
###Aim: To get the data into one dataset, and the flow as hourly.
###############

###First section will just load the two data files - quality & flow
	
	#load the flow data
flow<-read.csv("2122052.TXT",skip=2,header=T)
	#fix the dates up
flow$Date<-paste(as.POSIXlt(strptime(flow$Date,"%d/%m/%Y %H:%M")))
	#convert the character dates to posixlt
flow$Date<-as.POSIXlt(flow$Date)
flow<-data.frame(Date=as.POSIXlt(flow$Date),Total=flow$Total)
	#make sure it worked
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

###Now have to figure out how to put the samples together.






###This section will change the time stamp of the quality values to a 15min time scale and than the two can be joined. - In the sense that if the quality sample is taken within that hour of record flow, than that flow is associated with the quality sample.


###Do some summary stats, of the data, get some histograms, boxcox images and the like.

###Output the summary for the latex documents.
