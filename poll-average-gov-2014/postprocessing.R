library(rjson)

dir.create('post/', showWarnings=FALSE, recursive=TRUE)
if (file.exists("/var/www/html/pollster")) {
  dataDir <- '/var/www/html/pollster/shared/models/'
} else {
  dataDir <- 'data/'
}

#setting date for subset#
today <- as.Date(Sys.time(),tz="America/New_York")
electionday <- as.Date("2014-11-04")

##repeat over all states##
##no polls states to be added as needed: AL, HI, ID, NE, NV, OK, RI, SD, TN, VT, WY

##import data, put in correct format##
chart <- '2014-arizona-governor-ducey-vs-duval'
outAZ <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAZ <- outAZ[min(grep("minus",outAZ$who)):nrow(outAZ),] #this tells it to only import the "minus" data--which has the probability associated
outAZ$date2 <- as.Date(outAZ$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outAZ <- subset(outAZ, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outAZ$state<-"AZ"
outAZ$democrat<-"DuVal"
outAZ$republican<-"Ducey"
outAZ$lead<-ifelse(outAZ$who=="DuVal minus Ducey","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outAZ$numdays <- electionday - today #code number of days to election
outAZ$numpolls <- 0
write.csv(outAZ, file=paste('post/',chart,'.csv',sep='')) ##save file for merging later

chart <- '2014-arkansas-governor-hutchinson-vs-ross'
outAR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAR <- outAR[min(grep("minus",outAR$who)):nrow(outAR),] #this tells it to only import the "minus" data--which has the probability associated
outAR$date2 <- as.Date(outAR$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outAR <- subset(outAR, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outAR$state<-"AR"
outAR$democrat<- "Ross"
outAR$republican<-"Hutchinson"
outAR$lead<-ifelse(outAR$who=="Ross minus Hutchinson","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outAR$numdays <- electionday - today #code number of days to election
outAR$numpolls <- 0
write.csv(outAR, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-california-governor-kashkari-vs-brown'
outCA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCA <- outCA[min(grep("minus",outCA$who)):nrow(outCA),] #this tells it to only import the "minus" data--which has the probability associated
outCA$date2 <- as.Date(outCA$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outCA <- subset(outCA, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outCA$state<-"CA"
outCA$democrat<- "Brown"
outCA$republican<-"Kashkari"
outCA$lead<-ifelse(outCA$who=="Brown minus Kashkari","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outCA$numdays <- electionday - today #code number of days to election
outCA$numpolls <- 0
write.csv(outCA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-colorado-governor-beauprez-vs-hickenlooper'
outCO <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCO <- outCO[min(grep("minus",outCO$who)):nrow(outCO),] #this tells it to only import the "minus" data--which has the probability associated
outCO$date2 <- as.Date(outCO$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outCO <- subset(outCO, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outCO$state<-"CO"
outCO$democrat<- "Hickenlooper"
outCO$republican<-"Beauprez"
outCO$lead<-ifelse(outCO$who=="Hickenlooper minus Beauprez","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outCO$numdays <- electionday - today #code number of days to election
outCO$numpolls <- 0
write.csv(outCO, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-connecticut-governor-foley-vs-malloy'
outCT <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCT <- outCT[min(grep("minus",outCT$who)):nrow(outCT),] #this tells it to only import the "minus" data--which has the probability associated
outCT$date2 <- as.Date(outCT$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outCT <- subset(outCT, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outCT$state<-"CT"
outCT$democrat<- "Malloy"
outCT$republican<-"Foley"
outCT$lead<-ifelse(outCT$who=="Malloy minus Foley","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outCT$numdays <- electionday - today #code number of days to election
outCT$numpolls <- 0
write.csv(outCT, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-florida-governor-scott-vs-crist'
outFL <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outFL <- outFL[min(grep("minus",outFL$who)):nrow(outFL),] #this tells it to only import the "minus" data--which has the probability associated
outFL$date2 <- as.Date(outFL$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outFL <- subset(outFL, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outFL$state<-"FL"
outFL$democrat<- "Crist"
outFL$republican<-"Scott"
outFL$lead<-ifelse(outFL$who=="Crist minus Scott","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outFL$numdays <- electionday - today #code number of days to election
outFL$numpolls <- 0
write.csv(outFL, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-georgia-governor-deal-vs-carter'
outGA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outGA <- outGA[min(grep("minus",outGA$who)):nrow(outGA),] #this tells it to only import the "minus" data--which has the probability associated
outGA$date2 <- as.Date(outGA$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outGA <- subset(outGA, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outGA$state<-"GA"
outGA$democrat<- "Carter"
outGA$republican<-"Deal"
outGA$lead<-ifelse(outGA$who=="Carter minus Deal","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outGA$numdays <- electionday - today #code number of days to election
outGA$numpolls <- 0
write.csv(outGA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-illinois-governor-rauner-vs-quinn'
outIL <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIL <- outIL[min(grep("minus",outIL$who)):nrow(outIL),] #this tells it to only import the "minus" data--which has the probability associated
outIL$date2 <- as.Date(outIL$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outIL <- subset(outIL, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outIL$state<-"IL"
outIL$democrat<- "Quinn"
outIL$republican<-"Rauner"
outIL$lead<-ifelse(outIL$who=="Quinn minus Rauner","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outIL$numdays <- electionday - today #code number of days to election
outIL$numpolls <- 0
write.csv(outIL, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-iowa-governor-branstad-vs-hatch'
outIA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIA <- outIA[min(grep("minus",outIA$who)):nrow(outIA),] #this tells it to only import the "minus" data--which has the probability associated
outIA$date2 <- as.Date(outIA$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outIA <- subset(outIA, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outIA$state<-"IA"
outIA$democrat<- "Hatch"
outIA$republican<-"Branstad"
outIA$lead<-ifelse(outIA$who=="Hatch minus Branstad","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outIA$numdays <- electionday - today #code number of days to election
outIA$numpolls <- 0
write.csv(outIA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-kansas-governor-brownback-vs-davis'
outKS <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outKS <- outKS[min(grep("minus",outKS$who)):nrow(outKS),] #this tells it to only import the "minus" data--which has the probability associated
outKS$date2 <- as.Date(outKS$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outKS <- subset(outKS, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outKS$state<-"KS"
outKS$democrat<- "Davis"
outKS$republican<-"Brownback"
outKS$lead<-ifelse(outKS$who=="Davis minus Brownback","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outKS$numdays <- electionday - today #code number of days to election
outKS$numpolls <- 0
write.csv(outKS, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-maine-governor-lepage-vs-michaud-vs-cutler'
outME <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outME <- outME[min(grep("minus",outME$who)):nrow(outME),] #this tells it to only import the "minus" data--which has the probability associated
outME$date2 <- as.Date(outME$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outME <- subset(outME, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outME$state<-"ME"
outME$democrat<- "Michaud"
outME$republican<-"LePage"
outME$lead<-ifelse(outME$who=="Michaud minus LePage","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outME$numdays <- electionday - today #code number of days to election
outME$numpolls <- 0
write.csv(outME, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-maryland-governor-hogan-vs-brown'
outMD <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMD <- outMD[min(grep("minus",outMD$who)):nrow(outMD),] #this tells it to only import the "minus" data--which has the probability associated
outMD$date2 <- as.Date(outMD$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outMD <- subset(outMD, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outMD$state<-"MD"
outMD$democrat<- "Brown"
outMD$republican<-"Hogan"
outMD$lead<-ifelse(outMD$who=="Brown minus Hogan","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outMD$numdays <- electionday - today #code number of days to election
outMD$numpolls <- 0
write.csv(outMD, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-massachusetts-governor-baker-vs-coakley'
outMA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMA <- outMA[min(grep("minus",outMA$who)):nrow(outMA),] #this tells it to only import the "minus" data--which has the probability associated
outMA$date2 <- as.Date(outMA$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outMA <- subset(outMA, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outMA$state<-"MA"
outMA$democrat<- "Coakley"
outMA$republican<-"Baker"
outMA$lead<-ifelse(outMA$who=="Coakley minus Baker","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outMA$numdays <- electionday - today #code number of days to election
outMA$numpolls <- 0
write.csv(outMA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-michigan-governor-snyder-vs-schauer'
outMI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMI <- outMI[min(grep("minus",outMI$who)):nrow(outMI),] #this tells it to only import the "minus" data--which has the probability associated
outMI$date2 <- as.Date(outMI$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outMI <- subset(outMI, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outMI$state<-"MI"
outMI$democrat<- "Schauer"
outMI$republican<-"Snyder"
outMI$lead<-ifelse(outMI$who=="Schauer minus Snyder","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outMI$numdays <- electionday - today #code number of days to election
outMI$numpolls <- 0
write.csv(outMI, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-minnesota-governor-dayton-vs-johnson'
outMN <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMN <- outMN[min(grep("minus",outMN$who)):nrow(outMN),] #this tells it to only import the "minus" data--which has the probability associated
outMN$date2 <- as.Date(outMN$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outMN <- subset(outMN, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outMN$state<-"MN"
outMN$democrat<- "Dayton"
outMN$republican<-"Johnson"
outMN$lead<-ifelse(outMN$who=="Dayton minus Johnson","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outMN$numdays <- electionday - today #code number of days to election
outMN$numpolls <- 0
write.csv(outMN, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-new-hampshire-governor-havenstein-vs-hassan'
outNH <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNH <- outNH[min(grep("minus",outNH$who)):nrow(outNH),] #this tells it to only import the "minus" data--which has the probability associated
outNH$date2 <- as.Date(outNH$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outNH <- subset(outNH, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outNH$state<-"NH"
outNH$democrat<- "Hassan"
outNH$republican<-"Havenstein"
outNH$lead<-ifelse(outNH$who=="Hassan minus Havenstein","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outNH$numdays <- electionday - today #code number of days to election
outNH$numpolls <- 0
write.csv(outNH, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-new-mexico-governor-martinez-vs-king'
outNM <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNM <- outNM[min(grep("minus",outNM$who)):nrow(outNM),] #this tells it to only import the "minus" data--which has the probability associated
outNM$date2 <- as.Date(outNM$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outNM <- subset(outNM, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outNM$state<-"NM"
outNM$democrat<- "King"
outNM$republican<-"Martinez"
outNM$lead<-ifelse(outNM$who=="King minus Martinez","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outNM$numdays <- electionday - today #code number of days to election
outNM$numpolls <- 0
write.csv(outNM, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-new-york-governor-astorino-vs-cuomo'
outNY <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNY <- outNY[min(grep("minus",outNY$who)):nrow(outNY),] #this tells it to only import the "minus" data--which has the probability associated
outNY$date2 <- as.Date(outNY$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outNY <- subset(outNY, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outNY$state<-"NY"
outNY$democrat<- "Cuomo"
outNY$republican<-"Astorino"
outNY$lead<-ifelse(outNY$who=="Cuomo minus Astorino","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outNY$numdays <- electionday - today #code number of days to election
outNY$numpolls <- 0
write.csv(outNY, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-ohio-governor-kasich-vs-fitzgerald'
outOH <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOH <- outOH[min(grep("minus",outOH$who)):nrow(outOH),] #this tells it to only import the "minus" data--which has the probability associated
outOH$date2 <- as.Date(outOH$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outOH <- subset(outOH, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outOH$state<-"OH"
outOH$democrat<- "Fitzgerald"
outOH$republican<-"Kasich"
outOH$lead<-ifelse(outOH$who=="Fitzgerald minus Kasich","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outOH$numdays <- electionday - today #code number of days to election
outOH$numpolls <- 0
write.csv(outOH, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-oregon-governor-richardson-vs-kitzhaber'
outOR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOR <- outOR[min(grep("minus",outOR$who)):nrow(outOR),] #this tells it to only import the "minus" data--which has the probability associated
outOR$date2 <- as.Date(outOR$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outOR <- subset(outOR, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outOR$state<-"OR"
outOR$democrat<- "Kitzhaber"
outOR$republican<-"Richardson"
outOR$lead<-ifelse(outOR$who=="Kitzhaber minus Richardson","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outOR$numdays <- electionday - today #code number of days to election
outOR$numpolls <- 0
write.csv(outOR, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-pennsylvania-governor-corbett-vs-wolf'
outPA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outPA <- outPA[min(grep("minus",outPA$who)):nrow(outPA),] #this tells it to only import the "minus" data--which has the probability associated
outPA$date2 <- as.Date(outPA$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outPA <- subset(outPA, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outPA$state<-"PA"
outPA$democrat<- "Wolf"
outPA$republican<-"Corbett"
outPA$lead<-ifelse(outPA$who=="Wolf minus Corbett","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outPA$numdays <- electionday - today #code number of days to election
outPA$numpolls <- 0
write.csv(outPA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-governor-haley-vs-sheheen'
outSC <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSC <- outSC[min(grep("minus",outSC$who)):nrow(outSC),] #this tells it to only import the "minus" data--which has the probability associated
outSC$date2 <- as.Date(outSC$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outSC <- subset(outSC, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outSC$state<-"SC"
outSC$democrat<- "Sheheen"
outSC$republican<-"Haley"
outSC$lead<-ifelse(outSC$who=="Sheheen minus Haley","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outSC$numdays <- electionday - today #code number of days to election
outSC$numpolls <- 0
write.csv(outSC, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-texas-governor-abbott-vs-davis'
outTX <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outTX <- outTX[min(grep("minus",outTX$who)):nrow(outTX),] #this tells it to only import the "minus" data--which has the probability associated
outTX$date2 <- as.Date(outTX$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outTX <- subset(outTX, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outTX$state<-"TX"
outTX$democrat<- "Davis"
outTX$republican<-"Abbott"
outTX$lead<-ifelse(outTX$who=="Davis minus Abbott","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outTX$numdays <- electionday - today #code number of days to election
outTX$numpolls <- 0
write.csv(outTX, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-wisconsin-governor-walker-vs-burke'
outWI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outWI <- outWI[min(grep("minus",outWI$who)):nrow(outWI),] #this tells it to only import the "minus" data--which has the probability associated
outWI$date2 <- as.Date(outWI$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outWI <- subset(outWI, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outWI$state<-"WI"
outWI$democrat<- "Burke"
outWI$republican<-"Walker"
outWI$lead<-ifelse(outWI$who=="Burke minus Walker","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outWI$numdays <- electionday - today #code number of days to election
outWI$numpolls <- 0
write.csv(outWI, file=paste('post/',chart,'.csv',sep=''))

####Merge files into one#####
filenames <- list.files(path="post/", pattern='^2014.*csv', full.names=TRUE)
alldata<-do.call("rbind", lapply(filenames, read.csv, header=TRUE))
alldata$X.1<-NULL ##deletes defunct case number column
alldata$X<-NULL  ##deletes the other defunct case number column
alldata$date<-NULL ##deletes duplicate date column (date2 is in date format)
write.csv(alldata,"post/alldata.csv")

##Take only last row from each state
pollstates <- alldata[which(alldata$date2=="2014-11-04"),]
write.csv(pollstates,"post/pollstates.csv")

##Get undecided info for adjustment below
source("undecided.R")

##merge with nopolls-states.csv
nopollstates <- read.csv("nopolls-states.csv")
nopollstates$numdays <- electionday - today
nopollstates$date2 <- "2014-11-04"
allstates <- rbind(pollstates, nopollstates)
write.csv(allstates,"post/allstates.csv")

#make sure prob is numeric
allstates$prob <- as.numeric(allstates$prob)
allstates$call[allstates$lead=="Democrat lead" & allstates$prob2 >= 50] <- "D"
allstates$call[allstates$lead=="Republican lead" & allstates$prob2 >= 50] <- "R"
allstates$call[allstates$lead=="Democrat lead" & allstates$prob2 < 50] <- "R"
allstates$call[allstates$lead=="Republican lead" & allstates$prob2 < 50] <- "D"

#undecided adjustment
allstates$undecMargin <- abs(allstates$undecided/allstates$xibar)
allstates$undecMargin[allstates$undecMargin=="NaN"]<- 0
allstates$undecMargin[allstates$undecMargin > 10] <- 10 #don't allow values above 10

#adjustment to keep probability above 50 (prevent allowing the jittering to flip the race)
allstates$prob2ratio <- allstates$prob/allstates$prob2 #ratio of jittered to not jittered
allstates$finalprobA <- 50 + ((allstates$prob2 - 50)*allstates$prob2ratio) #multiply amount above 50 by ratio, add to 50
allstates$finalprob <- allstates$finalprobA - allstates$undecMargin #final probability
allstates$finalprob[allstates$finalprob <= 50] <- 50 #truncate at 50 to keep undecided adjustment from flipping the race
allstates$pollprob <- allstates$prob2

#write.csv(allstates,"allstates.csv") ##write the whole file only if you need to check everything

outGovernors14 <- allstates[, c("state", "call", "finalprob", "pollprob", "democrat", "republican", "numdays")]

write.csv(outGovernors14, "post/outGovernors14.csv")
write.csv(outGovernors14, paste("post/outGovernors14_",today,".csv",sep=""))
if (file.exists("/var/www/html/elections")) {
  write.csv(outGovernors14, "/var/www/html/pollster/shared/models/outGovernors14.csv")
  write.csv(outGovernors14, paste("/var/www/html/pollster/shared/models/outGovernors14_",today,".csv",sep=""))
  write.csv(outGovernors14, "/var/www/html/elections/shared/gov_2014/outGovernors14.csv")
  write.csv(outGovernors14, paste("/var/www/html/elections/shared/gov_2014/outGovernors14_",today,".csv",sep=""))
}

if (file.exists("/var/www/html/elections")) {
  system(paste("mkdir -p /var/www/html/pollster/shared/models/",today,"/post",sep=""))
  system(paste("cp post/2014-*.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
  system(paste("cp post/all*.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
  system(paste("cp post/pollstates.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
  system(paste("cp post/outGovernors14.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
}