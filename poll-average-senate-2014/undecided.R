dir.create('post/und/', showWarnings=FALSE, recursive=TRUE)
##no polls states to be added as needed: AL, DE, ID, MT(un-#), OK1, OK2, RI, WY

chart <- '2014-alaska-senate-sullivan-vs-begich'
outAK <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAK <- outAK[min(grep("Undecided",outAK$who)):nrow(outAK),]
outAK$date2 <- as.Date(outAK$date, format="%Y-%m-%d")
outAK <- subset(outAK, date2>today)
outAK$state<-"AK"
write.csv(outAK, file=paste('post/und/',chart,'.csv',sep='')) ##save file for merging later

chart <- '2014-arkansas-senate-cotton-vs-pryor'
outAR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAR <- outAR[min(grep("Undecided",outAR$who)):nrow(outAR),]
outAR$date2 <- as.Date(outAR$date, format="%Y-%m-%d")
outAR <- subset(outAR, date2>today)
outAR$state<-"AR"
write.csv(outAR, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-colorado-senate-gardner-vs-udall'
outCO <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCO <- outCO[min(grep("Undecided",outCO$who)):nrow(outCO),]
outCO$date2 <- as.Date(outCO$date, format="%Y-%m-%d")
outCO <- subset(outCO, date2>today)
outCO$state<-"CO"
write.csv(outCO, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-georgia-senate-perdue-vs-nunn'
outGA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outGA <- outGA[min(grep("Undecided",outGA$who)):nrow(outGA),]
outGA$date2 <- as.Date(outGA$date, format="%Y-%m-%d")
outGA <- subset(outGA, date2>today)
outGA$state<-"GA"
write.csv(outGA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-hawaii-senate-cavasso-vs-schatz'
outHI <- read.csv(paste('data/',chart,'/out.csv',sep=''))
outHI <- outHI[min(grep("Undecided",outHI$who)):nrow(outHI),]
outHI$date2 <- as.Date(outHI$date, format="%Y-%m-%d")
outHI <- subset(outHI, date2>today)
outHI$state<-"HI"
write.csv(outHI, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-iowa-senate-ernst-vs-braley'
outIA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIA <- outIA[min(grep("Undecided",outIA$who)):nrow(outIA),]
outIA$date2 <- as.Date(outIA$date, format="%Y-%m-%d")
outIA <- subset(outIA, date2>today)
outIA$state<-"IA"
write.csv(outIA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-illinois-senate-oberweis-vs-durbin'
outIL <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIL <- outIL[min(grep("Undecided",outIL$who)):nrow(outIL),]
outIL$date2 <- as.Date(outIL$date, format="%Y-%m-%d")
outIL <- subset(outIL, date2>today)
outIL$state<-"IL"
write.csv(outIL, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-kansas-senate-roberts-vs-orman-vs-taylor'
outKS <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outKS <- outKS[min(grep("Undecided",outKS$who)):nrow(outKS),]
outKS$date2 <- as.Date(outKS$date, format="%Y-%m-%d")
outKS <- subset(outKS, date2>today)
outKS$state<-"KS"
write.csv(outKS, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-kentucky-senate-mcconnell-vs-grimes'
outKY <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outKY <- outKY[min(grep("Undecided",outKY$who)):nrow(outKY),]
outKY$date2 <- as.Date(outKY$date, format="%Y-%m-%d")
outKY <- subset(outKY, date2>today)
outKY$state<-"KY"
write.csv(outKY, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-louisiana-senate-cassidy-vs-landrieu'
outLA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outLA <- outLA[min(grep("Undecided",outLA$who)):nrow(outLA),]
outLA$date2 <- as.Date(outLA$date, format="%Y-%m-%d")
outLA <- subset(outLA, date2>today)
outLA$state<-"LA"
write.csv(outLA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-maine-senate-collins-vs-bellows'
outME <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outME <- outME[min(grep("Undecided",outME$who)):nrow(outME),]
outME$date2 <- as.Date(outME$date, format="%Y-%m-%d")
outME <- subset(outME, date2>today)
outME$state<-"ME"
write.csv(outME, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-massachusetts-senate-herr-vs-markey'
outMA <- read.csv(paste('data/',chart,'/out.csv',sep=''))
outMA <- outMA[min(grep("Undecided",outMA$who)):nrow(outMA),]
outMA$date2 <- as.Date(outMA$date, format="%Y-%m-%d")
outMA <- subset(outMA, date2>today)
outMA$state<-"MA"
write.csv(outMA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-michigan-senate-land-vs-peters'
outMI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMI <- outMI[min(grep("Undecided",outMI$who)):nrow(outMI),]
outMI$date2 <- as.Date(outMI$date, format="%Y-%m-%d")
outMI <- subset(outMI, date2>today)
outMI$state<-"MI"
write.csv(outMI, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-minnesota-senate-mcfadden-vs-franken'
outMN <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMN <- outMN[min(grep("Undecided",outMN$who)):nrow(outMN),]
outMN$date2 <- as.Date(outMN$date, format="%Y-%m-%d")
outMN <- subset(outMN, date2>today)
outMN$state<-"MN"
write.csv(outMN, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-mississippi-senate-cochran-vs-childers'
outMS <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMS <- outMS[min(grep("Undecided",outMS$who)):nrow(outMS),]
outMS$date2 <- as.Date(outMS$date, format="%Y-%m-%d")
outMS <- subset(outMS, date2>today)
outMS$state<-"MS"
write.csv(outMS, file=paste('post/und/',chart,'.csv',sep=''))

#chart <- '2014-montana-senate-daines-vs-walsh'
#outMT <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
#outMT <- outMT[min(grep("Undecided",outMT$who)):nrow(outMT),]
#outMT$date2 <- as.Date(outMT$date, format="%Y-%m-%d")
#outMT <- subset(outMT, date2>today)
#outMT$state<-"MT"
#write.csv(outMT, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-nebraska-senate-sasse-vs-domina'
outNE <- read.csv(paste('data/',chart,'/out.csv',sep=''))
outNE <- outNE[min(grep("Undecided",outNE$who)):nrow(outNE),]
outNE$date2 <- as.Date(outNE$date, format="%Y-%m-%d")
outNE <- subset(outNE, date2>today)
outNE$state<-"NE"
write.csv(outNE, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-north-carolina-senate-tillis-vs-hagan'
outNC <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNC <- outNC[min(grep("Undecided",outNC$who)):nrow(outNC),]
outNC$date2 <- as.Date(outNC$date, format="%Y-%m-%d")
outNC <- subset(outNC, date2>today)
outNC$state<-"NC"
write.csv(outNC, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-new-hampshire-senate-brown-vs-shaheen'
outNH <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNH <- outNH[min(grep("Undecided",outNH$who)):nrow(outNH),]
outNH$date2 <- as.Date(outNH$date, format="%Y-%m-%d")
outNH <- subset(outNH, date2>today)
outNH$state<-"NH"
write.csv(outNH, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-new-jersey-senate-bell-vs-booker'
outNJ <- read.csv(paste('data/',chart,'/out.csv',sep=''))
outNJ <- outNJ[min(grep("Undecided",outNJ$who)):nrow(outNJ),]
outNJ$date2 <- as.Date(outNJ$date, format="%Y-%m-%d")
outNJ <- subset(outNJ, date2>today)
outNJ$state<-"NJ"
write.csv(outNJ, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-new-mexico-senate-weh-vs-udall'
outNM <- read.csv(paste('data/',chart,'/out.csv',sep=''))
outNM <- outNM[min(grep("Undecided",outNM$who)):nrow(outNM),]
outNM$date2 <- as.Date(outNM$date, format="%Y-%m-%d")
outNM <- subset(outNM, date2>today)
outNM$state<-"NM"
write.csv(outNM, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-oregon-senate-wehby-vs-merkley'
outOR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOR <- outOR[min(grep("Undecided",outOR$who)):nrow(outOR),]
outOR$date2 <- as.Date(outOR$date, format="%Y-%m-%d")
outOR <- subset(outOR, date2>today)
outOR$state<-"OR"
write.csv(outOR, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-senate-graham-vs-hutto'
outSC1 <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSC1 <- outSC1[min(grep("Undecided",outSC1$who)):nrow(outSC1),]
outSC1$date2 <- as.Date(outSC1$date, format="%Y-%m-%d")
outSC1 <- subset(outSC1, date2>today)
outSC1$state<-"SC1"
write.csv(outSC1, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-senate-scott-vs-dickerson'
outSC2 <- read.csv(paste('data/',chart,'/out.csv',sep=''))
outSC2 <- outSC2[min(grep("Undecided",outSC2$who)):nrow(outSC2),]
outSC2$date2 <- as.Date(outSC2$date, format="%Y-%m-%d")
outSC2 <- subset(outSC2, date2>today)
outSC2$state<-"SC2"
write.csv(outSC2, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-south-dakota-senate-rounds-vs-weiland'
outSD <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSD <- outSD[min(grep("Undecided",outSD$who)):nrow(outSD),]
outSD$date2 <- as.Date(outSD$date, format="%Y-%m-%d")
outSD <- subset(outSD, date2>today)
outSD$state<-"SD"
write.csv(outSD, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-tennessee-senate-alexander-vs-ball'
outTN <- read.csv(paste('data/',chart,'/out.csv',sep=''))
outTN <- outTN[min(grep("Undecided",outTN$who)):nrow(outTN),]
outTN$date2 <- as.Date(outTN$date, format="%Y-%m-%d")
outTN <- subset(outTN, date2>today)
outTN$state<-"TN"
write.csv(outTN, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-texas-senate-cornyn-vs-alameel'
outTX <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outTX <- outTX[min(grep("Undecided",outTX$who)):nrow(outTX),]
outTX$date2 <- as.Date(outTX$date, format="%Y-%m-%d")
outTX <- subset(outTX, date2>today)
outTX$state<-"TX"
write.csv(outTX, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-virginia-senate-gillespie-vs-warner'
outVA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outVA <- outVA[min(grep("Undecided",outVA$who)):nrow(outVA),]
outVA$date2 <- as.Date(outVA$date, format="%Y-%m-%d")
outVA <- subset(outVA, date2>today)
outVA$state<-"VA"
write.csv(outVA, file=paste('post/und/',chart,'.csv',sep=''))

chart <- '2014-west-virginia-senate-capito-vs-tennant'
outWV <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outWV <- outWV[min(grep("Undecided",outWV$who)):nrow(outWV),]
outWV$date2 <- as.Date(outWV$date, format="%Y-%m-%d")
outWV <- subset(outWV, date2>today)
outWV$state<-"WV"
write.csv(outWV, file=paste('post/und/',chart,'.csv',sep=''))


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

pollstates$undecided <- undecided$xibar
