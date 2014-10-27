dir.create('post/und/', showWarnings=FALSE, recursive=TRUE)
##no polls states to be added as needed: AL, HI, ID, NE, NV, OK, RI, SD, TN, VT, WY

chart <- '2014-alaska-governor-parnell-vs-walker'
outAK <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAK <- outAK[min(grep("Undecided",outAK$who)):nrow(outAK),]
outAK$date2 <- as.Date(outAK$date, format="%Y-%m-%d")
outAK <- subset(outAK, date2>today)
outAK$state<-"AK"
write.csv(outAK, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-alabama-governor-bentley-vs-griffith'
outAL <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAL <- outAL[min(grep("Undecided",outAL$who)):nrow(outAL),]
outAL$date2 <- as.Date(outAL$date, format="%Y-%m-%d")
outAL <- subset(outAL, date2>today)
outAL$state<-"AL"
write.csv(outAL, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-arizona-governor-ducey-vs-duval'
outAZ <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAZ <- outAZ[min(grep("Undecided",outAZ$who)):nrow(outAZ),]
outAZ$date2 <- as.Date(outAZ$date, format="%Y-%m-%d")
outAZ <- subset(outAZ, date2>today)
outAZ$state<-"AZ"
write.csv(outAZ, file=paste('post/und/',chart,'.csv',sep='')) ##save file for merging later

chart <- '2014-arkansas-governor-hutchinson-vs-ross'
outAR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAR <- outAR[min(grep("Undecided",outAR$who)):nrow(outAR),]
outAR$date2 <- as.Date(outAR$date, format="%Y-%m-%d")
outAR <- subset(outAR, date2>today)
outAR$state<-"AR"
write.csv(outAR, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-california-governor-kashkari-vs-brown'
outCA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCA <- outCA[min(grep("Undecided",outCA$who)):nrow(outCA),]
outCA$date2 <- as.Date(outCA$date, format="%Y-%m-%d")
outCA <- subset(outCA, date2>today)
outCA$state<-"CA"
write.csv(outCA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-colorado-governor-beauprez-vs-hickenlooper'
outCO <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCO <- outCO[min(grep("Undecided",outCO$who)):nrow(outCO),]
outCO$date2 <- as.Date(outCO$date, format="%Y-%m-%d")
outCO <- subset(outCO, date2>today)
outCO$state<-"CO"
write.csv(outCO, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-connecticut-governor-foley-vs-malloy'
outCT <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCT <- outCT[min(grep("Undecided",outCT$who)):nrow(outCT),]
outCT$date2 <- as.Date(outCT$date, format="%Y-%m-%d")
outCT <- subset(outCT, date2>today)
outCT$state<-"CT"
write.csv(outCT, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-florida-governor-scott-vs-crist'
outFL <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outFL <- outFL[min(grep("Undecided",outFL$who)):nrow(outFL),]
outFL$date2 <- as.Date(outFL$date, format="%Y-%m-%d")
outFL <- subset(outFL, date2>today)
outFL$state<-"FL"
write.csv(outFL, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-georgia-governor-deal-vs-carter'
outGA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outGA <- outGA[min(grep("Undecided",outGA$who)):nrow(outGA),]
outGA$date2 <- as.Date(outGA$date, format="%Y-%m-%d")
outGA <- subset(outGA, date2>today)
outGA$state<-"GA"
write.csv(outGA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-hawaii-governor-aiona-vs-ige'
outHI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outHI <- outHI[min(grep("Undecided",outHI$who)):nrow(outHI),] 
outHI$date2 <- as.Date(outHI$date, format="%Y-%m-%d") 
outHI <- subset(outHI, date2>today) 
outHI$state<-"HI"
write.csv(outHI, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-idaho-governor-otter-vs-balukoff'
outID <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outID <- outID[min(grep("Undecided",outID$who)):nrow(outID),] 
outID$date2 <- as.Date(outID$date, format="%Y-%m-%d") 
outID <- subset(outID, date2>today) 
outID$state<-"ID"
write.csv(outID, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-illinois-governor-rauner-vs-quinn'
outIL <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIL <- outIL[min(grep("Undecided",outIL$who)):nrow(outIL),]
outIL$date2 <- as.Date(outIL$date, format="%Y-%m-%d")
outIL <- subset(outIL, date2>today)
outIL$state<-"IL"
write.csv(outIL, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-iowa-governor-branstad-vs-hatch'
outIA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIA <- outIA[min(grep("Undecided",outIA$who)):nrow(outIA),]
outIA$date2 <- as.Date(outIA$date, format="%Y-%m-%d")
outIA <- subset(outIA, date2>today)
outIA$state<-"IA"
write.csv(outIA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-kansas-governor-brownback-vs-davis'
outKS <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outKS <- outKS[min(grep("Undecided",outKS$who)):nrow(outKS),]
outKS$date2 <- as.Date(outKS$date, format="%Y-%m-%d")
outKS <- subset(outKS, date2>today)
outKS$state<-"KS"
write.csv(outKS, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-maine-governor-lepage-vs-michaud-vs-cutler'
outME <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outME <- outME[min(grep("Undecided",outME$who)):nrow(outME),]
outME$date2 <- as.Date(outME$date, format="%Y-%m-%d")
outME <- subset(outME, date2>today)
outME$state<-"ME"
write.csv(outME, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-maryland-governor-hogan-vs-brown'
outMD <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMD <- outMD[min(grep("Undecided",outMD$who)):nrow(outMD),]
outMD$date2 <- as.Date(outMD$date, format="%Y-%m-%d")
outMD <- subset(outMD, date2>today)
outMD$state<-"MD"
write.csv(outMD, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-massachusetts-governor-baker-vs-coakley'
outMA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMA <- outMA[min(grep("Undecided",outMA$who)):nrow(outMA),]
outMA$date2 <- as.Date(outMA$date, format="%Y-%m-%d")
outMA <- subset(outMA, date2>today)
outMA$state<-"MA"
write.csv(outMA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-michigan-governor-snyder-vs-schauer'
outMI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMI <- outMI[min(grep("Undecided",outMI$who)):nrow(outMI),]
outMI$date2 <- as.Date(outMI$date, format="%Y-%m-%d")
outMI <- subset(outMI, date2>today)
outMI$state<-"MI"
write.csv(outMI, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-minnesota-governor-dayton-vs-johnson'
outMN <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMN <- outMN[min(grep("Undecided",outMN$who)):nrow(outMN),]
outMN$date2 <- as.Date(outMN$date, format="%Y-%m-%d")
outMN <- subset(outMN, date2>today)
outMN$state<-"MN"
write.csv(outMN, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-nebraska-governor-ricketts-vs-hassebrook'
outNE <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNE <- outNE[min(grep("Undecided",outNE$who)):nrow(outNE),]
outNE$date2 <- as.Date(outNE$date, format="%Y-%m-%d")
outNE <- subset(outNE, date2>today)
outNE$state<-"NE"
write.csv(outNE, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-new-hampshire-governor-havenstein-vs-hassan'
outNH <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNH <- outNH[min(grep("Undecided",outNH$who)):nrow(outNH),]
outNH$date2 <- as.Date(outNH$date, format="%Y-%m-%d")
outNH <- subset(outNH, date2>today)
outNH$state<-"NH"
write.csv(outNH, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-new-mexico-governor-martinez-vs-king'
outNM <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNM <- outNM[min(grep("Undecided",outNM$who)):nrow(outNM),]
outNM$date2 <- as.Date(outNM$date, format="%Y-%m-%d")
outNM <- subset(outNM, date2>today)
outNM$state<-"NM"
write.csv(outNM, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-nevada-governor-sandoval-vs-goodman'
outNV <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNV <- outNV[min(grep("Undecided",outNV$who)):nrow(outNV),]
outNV$date2 <- as.Date(outNV$date, format="%Y-%m-%d")
outNV <- subset(outNV, date2>today)
outNV$state<-"NV"
write.csv(outNV, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-new-york-governor-astorino-vs-cuomo'
outNY <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNY <- outNY[min(grep("Undecided",outNY$who)):nrow(outNY),]
outNY$date2 <- as.Date(outNY$date, format="%Y-%m-%d")
outNY <- subset(outNY, date2>today)
outNY$state<-"NY"
write.csv(outNY, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-ohio-governor-kasich-vs-fitzgerald'
outOH <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOH <- outOH[min(grep("Undecided",outOH$who)):nrow(outOH),]
outOH$date2 <- as.Date(outOH$date, format="%Y-%m-%d")
outOH <- subset(outOH, date2>today)
outOH$state<-"OH"
write.csv(outOH, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-oklahoma-governor-fallin-vs-dorman'
outOK <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOK <- outOK[min(grep("Undecided",outOK$who)):nrow(outOK),]
outOK$date2 <- as.Date(outOK$date, format="%Y-%m-%d")
outOK <- subset(outOK, date2>today)
outOK$state<-"OK"
write.csv(outOK, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-oregon-governor-richardson-vs-kitzhaber'
outOR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOR <- outOR[min(grep("Undecided",outOR$who)):nrow(outOR),]
outOR$date2 <- as.Date(outOR$date, format="%Y-%m-%d")
outOR <- subset(outOR, date2>today)
outOR$state<-"OR"
write.csv(outOR, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-pennsylvania-governor-corbett-vs-wolf'
outPA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outPA <- outPA[min(grep("Undecided",outPA$who)):nrow(outPA),]
outPA$date2 <- as.Date(outPA$date, format="%Y-%m-%d")
outPA <- subset(outPA, date2>today)
outPA$state<-"PA"
write.csv(outPA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-rhode-island-governor-fung-vs-raimondo'
outRI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outRI <- outRI[min(grep("Undecided",outRI$who)):nrow(outRI),] 
outRI$date2 <- as.Date(outRI$date, format="%Y-%m-%d") 
outRI <- subset(outRI, date2>today) 
outRI$state<-"RI"
write.csv(outRI, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-governor-haley-vs-sheheen'
outSC <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSC <- outSC[min(grep("Undecided",outSC$who)):nrow(outSC),]
outSC$date2 <- as.Date(outSC$date, format="%Y-%m-%d")
outSC <- subset(outSC, date2>today)
outSC$state<-"SC"
write.csv(outSC, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-south-dakota-governor-daugaard-vs-wismer'
outSD <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSD <- outSD[min(grep("Undecided",outSD$who)):nrow(outSD),]
outSD$date2 <- as.Date(outSD$date, format="%Y-%m-%d")
outSD <- subset(outSD, date2>today)
outSD$state<-"SD"
write.csv(outSD, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-tennessee-governor-haslam-vs-brown'
outTN <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outTN <- outTN[min(grep("Undecided",outTN$who)):nrow(outTN),]
outTN$date2 <- as.Date(outTN$date, format="%Y-%m-%d")
outTN <- subset(outTN, date2>today)
outTN$state<-"TN"
write.csv(outTN, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-texas-governor-abbott-vs-davis'
outTX <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outTX <- outTX[min(grep("Undecided",outTX$who)):nrow(outTX),]
outTX$date2 <- as.Date(outTX$date, format="%Y-%m-%d")
outTX <- subset(outTX, date2>today)
outTX$state<-"TX"
write.csv(outTX, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-vermont-governor-milne-vs-shumlin'
outVT <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outVT <- outVT[min(grep("Undecided",outVT$who)):nrow(outVT),]
outVT$date2 <- as.Date(outVT$date, format="%Y-%m-%d")
outVT <- subset(outVT, date2>today)
outVT$state<-"VT"
write.csv(outVT, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-wisconsin-governor-walker-vs-burke'
outWI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outWI <- outWI[min(grep("Undecided",outWI$who)):nrow(outWI),]
outWI$date2 <- as.Date(outWI$date, format="%Y-%m-%d")
outWI <- subset(outWI, date2>today)
outWI$state<-"WI"
write.csv(outWI, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-wyoming-governor'
outWY <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outWY <- outWY[min(grep("Undecided",outWY$who)):nrow(outWY),]
outWY$date2 <- as.Date(outWY$date, format="%Y-%m-%d")
outWY <- subset(outWY, date2>today)
outWY$state<-"WY"
write.csv(outWY, file=paste('post/und/',chart,'.csv',sep=''))

####Merge files into one#####
filenames <- list.files(path="post/und/", pattern='2014.*csv', full.names=TRUE)
alldata<-do.call("rbind", lapply(filenames, read.csv, header=TRUE))
alldata$X.1<-NULL ##deletes defunct case number column
alldata$X<-NULL  ##deletes the other defunct case number column
alldata$date<-NULL ##deletes duplicate date column (date2 is in date format)
alldata <- alldata[which(alldata$who=="Undecided"), ]
write.csv(alldata,"post/und/alldata.csv")

##Take only last row from each state
undecided <- alldata[which(alldata$date2=="2014-11-04"),]
write.csv(undecided,"post/und/pollstates.csv")

allstates$undecided <- undecided$xibar
