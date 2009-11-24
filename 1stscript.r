###############
###Title: Random code to get the data orgainsed and in the right format.
###Date Created: 16/11/09
###Aim: To get the data into one dataset, and the flow as hourly.
###############

###First section will just load the two data files - quality & flow
	
	#load the flow data
flow<-read.csv("212270_JoorilandFlow.csv",header=T,)
#convert the flow into the time series format.
flow2<-timeseries(flow$Date,"%d/%m/%Y %H:%M",flow$Total)
#look at the timeseries formatted data
head(flow2)
#Now aggregate the data into hourly flow. - getting rid of any hours that have not got complete observations
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
qual <- data.frame(Date=as.POSIXlt(qual$Date)+minutes*60,sample=t[,2],pH=t[,3],EC=t[,4],NTU=t[,5],TN=t[,6],TP=t[,7],Total=t[,8])
#check the new data frame out
head(qual)
#It seems to be working...

###change the time stamp, that changes times before 7am to midday.start with using test dataset
for(i in 1:length(qual[,1])){
  if(as.numeric(format(as.POSIXlt(qual$Date[i]),"%H"))<7){
    qual$Date[i] <- strptime(paste(as.numeric(format(as.POSIXlt(qual$Date[i]),"%d")),as.numeric(format(as.POSIXlt(qual$Date[i]),"%m")),as.numeric(format(as.POSIXlt(qual$Date[i]),"%Y")),12,0),"%d %m %Y %H %M")
    }
}

####Histogram time.
pdf("flowhist.pdf")
hist(qual$Total,main="Histogram of discharge at sampling time",xlab=(expression(paste("Discharge (", ML^-1, " hour)"))))
dev.off()
pdf("tphist.pdf")
hist(qual$TP,main="Histogram of total Phosphorus",xlab=expression(paste("Total Phosphours (",mg^-1," L)")))
dev.off()

###Get rid of samples without flow
qual <- subset(qual,!is.na(Total))
####Summary Time - this was done by hand with xtable - much better way of doing it.
summary(qual)
sd(qual,na.rm=T)

#fix the NTU
qual$NTU[17] <- NA
qual$NTU[12] <- NA

pdf("Scatterplotmatrix.pdf")
pairs(~log(Total+0.1)+log(TP+0.1)+log(TN+0.1)+log(NTU+0.1)+log(EC+0.1),data=qual,labels=c("Discharge","TP","TN","NTU","EC"))
dev.off()

####Simple correlation plot
plot(log(qual$Total+1),log(qual$TP+0.1))
temp <- subset(qual,!is.na(qual$Total))
temp <- subset(temp,!is.na(temp$TP))
tp <- log(temp$TP+0.1)
total <- log(temp$Total+1)
abline(glm(tp~total))

###Make a table showing how many of each sample type was collected for each variable
Summ <- summary(qual~sample,data=qual)


test <- subset(qual,!is.na(pH))
pH<-summary(test$sample)
test <- subset(qual,!is.na(EC))
EC<-summary(test$sample)
test <- subset(qual,!is.na(TP))
TP<-summary(test$sample)
test <- subset(qual,!is.na(TN))
TN<-summary(test$sample)
test <- subset(qual,!is.na(NTU))
NTU<-summary(test$sample)
#load the xtable library to get this into latex table format
library(xtable)
xtable(as.data.frame((cbind(pH,EC,TP,TN,NTU))))
#load the ggplot2 library
library(ggplot2)
q <- ggplot(log(qual$Total+0.1),log(qual$TP+0.1))
q+geom_point(color=qual$sample)

#Load the lattice library
library(lattice)
#Remove NA's from TP column
temp <- subset(qual,!is.na(TP))
#Remove N sample type as there is not enough to make an abline
temp <- subset(temp,sample!="N"|sample!="W")
#Change 'blank' to Unknown
temp$sample <- as.character(temp$sample)
for(i in 1:length(temp[,1])){
   if(temp$sample[i]=="")
    temp$sample[i]="Unknown"
   if(temp$sample[i]=="A")
    temp$sample[i]="Automatic"
   if(temp$sample[i]=="F")
    temp$sample[i]="Flood"
   if(temp$sample[i]=="R")
    temp$sample[i]="Monthly"
   if(temp$sample[i]=="D")
    temp$sample[i]="Duplicate"
   if(temp$sample[i]=="S")
    temp$sample[i]="Ad-hoc grab"
   if(temp$sample[i]=="C")
    temp$sample[i]="Event grab"
  }
temp$sample <- as.factor(temp$sample)

pdf("sample_tp_scatter.pdf")
xyplot(log(TP+0.1)~log(Total+0.1)|sample,data=temp,panel=function(x,y){
  panel.xyplot(x,y)
  panel.abline(lm(y~x))
}
       )
dev.off()

#Make some pics of the box cox transformation.
library(MASS)
temp <- subset(qual,!is.na(TP))
library(geoR)
pdf("bc_example_tp.pdf")
boxcox(temp$TP+0.01~1)
dev.off()
boxcox.fit(temp$TP+0.01)

##time to show the denisty plots of eacvh variable
pdf("histograms.pdf")
par(mfrow=c(3,2))
hist(qual$TP,xlab="Total Phosphorous",main="Histogram of Phosphorous")
hist(qual$TN,xlab="Total Nitrogen",main="Histogram of Nitrogen")
hist(qual$NTU,xlab="Turbidity", main= "Turbidity")
hist(qual$EC,xlab="EC",main="Histogram of EC")
hist(qual$pH,xlab="pH",main="Histogram of pH")
hist(qual$Total,xlab="Discharge",main="Histogram of discharge at during sampling")
dev.off()

##Need  to do flow acf pacf.
head(flow)
flow2 <- subset(flow,!is.na(Data))
pdf("acf_pacf.pdf")
par(mfrow=c(2,1))
acf(flow2$Data,main="ACF of all dichagre values")
pacf(flow2$Data,main="PACF of all discharge values")
dev.off()
##Need to do semivariograms with clouds for both flow and all the quality variables.
library(geoR)
library(TeachingDemos)
# change the dates to real numbers
temp <- qual
num <- qual[,3]
#Change the dates to distance (days) of each value from the first.
for(i in 1:length(temp[,1])){
  num[i]<- (as.numeric(temp$Date[i]-temp$Date[1]))
}
#Create a data frame for all the data
dataa <- data.frame(X=num,Y=1,qual[,4:8])
#Remove duplicated dates
dataa <- subset(dataa,!duplicated(dataa[,1]))
dataa <- subset(dataa,!is.na(TP))
names(dataa)
lam <- boxcox.fit(dataa$TP+0.01)
dataa$TP <- bct(dataa$TP+0.01,lam$lambda)
#create a spatial object for the next few steps
temp <- as.geodata(dataa,coords.col=1:2,data.col=6)
v.tp <- variog(temp,option="cloud")
mlik <- likfit(temp,ini=c(0.5,0.5))
pdf("TP_cloud.pdf")
plot(v.tp,main="Semi-variogram cloud of TP (boxcox transformed lambda=-0.40)")
lines(mlik,col="red")
dev.off()


#Now for the flow

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
