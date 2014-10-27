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
##no polls states to be added as needed: AL, RI

##import data, put in correct format##
chart <- '2014-alaska-senate-sullivan-vs-begich'
outAK <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAK <- outAK[min(grep("minus",outAK$who)):nrow(outAK),] #this tells it to only import the "minus" data--which has the probability associated
outAK$date2 <- as.Date(outAK$date, format="%Y-%m-%d") #date read in as a factor, convert to date for subsetting
outAK <- subset(outAK, date2>today) #deletes rows prior to today so that all files will have the same number of rows
outAK$state<-"AK"
outAK$democrat<-"Begich"
outAK$republican<-"Sullivan"
outAK$lead<-ifelse(outAK$who=="Begich minus Sullivan","Democrat lead", "Republican lead") ##code whether probability shows Dem lead or Rep lead
outAK$numdays <- electionday - today #code number of days to election
outAK$numpolls <- 0 ##number of polls
write.csv(outAK, file=paste('post/',chart,'.csv',sep='')) ##save file for merging later

chart <- '2014-arkansas-senate-cotton-vs-pryor'
outAR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outAR <- outAR[min(grep("minus",outAR$who)):nrow(outAR),]
outAR$date2 <- as.Date(outAR$date, format="%Y-%m-%d")
outAR <- subset(outAR, date2>today)
outAR$state<-"AR"
outAR$democrat<-"Pryor"
outAR$republican<-"Cotton"
outAR$lead<-ifelse(outAR$who=="Pryor minus Cotton","Democrat lead", "Republican lead")
outAR$numdays <- electionday - today
outAR$numpolls <- 0
write.csv(outAR, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-colorado-senate-gardner-vs-udall'
outCO <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outCO <- outCO[min(grep("minus",outCO$who)):nrow(outCO),]
outCO$date2 <- as.Date(outCO$date, format="%Y-%m-%d")
outCO <- subset(outCO, date2>today)
outCO$state<-"CO"
outCO$democrat<-"Udall"
outCO$republican<-"Gardner"
outCO$lead<-ifelse(outCO$who=="Udall minus Gardner","Democrat lead", "Republican lead")
outCO$numdays <- electionday - today
outCO$numpolls <- 0
write.csv(outCO, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-delaware-senate-wade-vs-coons'
outDE <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outDE <- outDE[min(grep("minus",outDE$who)):nrow(outDE),]
outDE$date2 <- as.Date(outDE$date, format="%Y-%m-%d")
outDE <- subset(outDE, date2>today)
outDE$state<-"DE"
outDE$democrat<-"Coons"
outDE$republican<-"Wade"
outDE$lead<-ifelse(outDE$who=="Coons minus Wade","Democrat lead", "Republican lead")
outDE$numdays <- electionday - today
outDE$numpolls <- 0
write.csv(outDE, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-georgia-senate-perdue-vs-nunn'
outGA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outGA <- outGA[min(grep("minus",outGA$who)):nrow(outGA),]
outGA$date2 <- as.Date(outGA$date, format="%Y-%m-%d")
outGA <- subset(outGA, date2>today)
outGA$state<-"GA"
outGA$democrat<-"Nunn"
outGA$republican<-"Perdue"
outGA$lead<-ifelse(outGA$who=="Nunn minus Perdue","Democrat lead", "Republican lead")
outGA$numdays <- electionday - today
outGA$numpolls <- 0
write.csv(outGA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-georgia-senate-runoff'
outGAR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outGAR <- outGAR[min(grep("minus",outGAR$who)):nrow(outGAR),]
outGAR$date2 <- as.Date(outGAR$date, format="%Y-%m-%d")
outGAR <- subset(outGAR, date2>today)
outGAR$state<-"GA-runoff"
outGAR$democrat<-"Nunn"
outGAR$republican<-"Perdue"
outGAR$lead<-ifelse(outGAR$who=="Nunn minus Perdue","Democrat lead", "Republican lead")
outGAR$numdays <- electionday - today
outGAR$numpolls <- 0
write.csv(outGAR, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-hawaii-senate-cavasso-vs-schatz'
outHI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outHI <- outHI[min(grep("minus",outHI$who)):nrow(outHI),]
outHI$date2 <- as.Date(outHI$date, format="%Y-%m-%d")
outHI <- subset(outHI, date2>today)
outHI$state<-"HI"
outHI$democrat<-"Schatz"
outHI$republican<-"Cavasso"
outHI$lead<-ifelse(outHI$who=="Schatz minus Cavasso","Democrat lead", "Republican lead")
outHI$numdays <- electionday - today
outHI$numpolls <- 0
write.csv(outHI, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-iowa-senate-ernst-vs-braley'
outIA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIA <- outIA[min(grep("minus",outIA$who)):nrow(outIA),]
outIA$date2 <- as.Date(outIA$date, format="%Y-%m-%d")
outIA <- subset(outIA, date2>today)
outIA$state<-"IA"
outIA$democrat<-"Braley"
outIA$republican<-"Ernst"
outIA$lead<-ifelse(outIA$who=="Braley minus Ernst","Democrat lead", "Republican lead")
outIA$numdays <- electionday - today
outIA$numpolls <- 0
write.csv(outIA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-idaho-senate-risch-vs-mitchell'
outID <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outID <- outID[min(grep("minus",outID$who)):nrow(outID),]
outID$date2 <- as.Date(outID$date, format="%Y-%m-%d")
outID <- subset(outID, date2>today)
outID$state<-"ID"
outID$democrat<-"Mitchell"
outID$republican<-"Risch"
outID$lead<-ifelse(outID$who=="Mitchell minus Risch","Democrat lead", "Republican lead")
outID$numdays <- electionday - today
outID$numpolls <- 0
write.csv(outID, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-illinois-senate-oberweis-vs-durbin'
outIL <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outIL <- outIL[min(grep("minus",outIL$who)):nrow(outIL),]
outIL$date2 <- as.Date(outIL$date, format="%Y-%m-%d")
outIL <- subset(outIL, date2>today)
outIL$state<-"IL"
outIL$democrat<-"Durbin"
outIL$republican<-"Oberweis"
outIL$lead<-ifelse(outIL$who=="Durbin minus Oberweis","Democrat lead", "Republican lead")
outIL$numdays <- electionday - today
outIL$numpolls <- 0
write.csv(outIL, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-kansas-senate-roberts-vs-orman-vs-taylor'
outKS <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outKS <- outKS[min(grep("minus",outKS$who)):nrow(outKS),]
outKS$date2 <- as.Date(outKS$date, format="%Y-%m-%d")
outKS <- subset(outKS, date2>today)
outKS$state<-"KS"
outKS$democrat<-"Orman"
outKS$republican<-"Roberts"
outKS$lead<-ifelse(outKS$who=="Orman minus Roberts","Independent lead", "Republican lead")
outKS$numdays <- electionday - today
outKS$numpolls <- 0
write.csv(outKS, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-kentucky-senate-mcconnell-vs-grimes'
outKY <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outKY <- outKY[min(grep("minus",outKY$who)):nrow(outKY),]
outKY$date2 <- as.Date(outKY$date, format="%Y-%m-%d")
outKY <- subset(outKY, date2>today)
outKY$state<-"KY"
outKY$democrat<-"Grimes"
outKY$republican<-"McConnell"
outKY$lead<-ifelse(outKY$who=="Grimes minus McConnell","Democrat lead", "Republican lead")
outKY$numdays <- electionday - today
outKY$numpolls <- 0
write.csv(outKY, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-louisiana-senate-cassidy-vs-landrieu'
outLA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outLA <- outLA[min(grep("minus",outLA$who)):nrow(outLA),]
outLA$date2 <- as.Date(outLA$date, format="%Y-%m-%d")
outLA <- subset(outLA, date2>today)
outLA$state<-"LA"
outLA$democrat<-"Landrieu"
outLA$republican<-"Cassidy"
outLA$lead<-ifelse(outLA$who=="Landrieu minus Cassidy","Democrat lead", "Republican lead")
outLA$numdays <- electionday - today
outLA$numpolls <- 0
write.csv(outLA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-maine-senate-collins-vs-bellows'
outME <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outME <- outME[min(grep("minus",outME$who)):nrow(outME),]
outME$date2 <- as.Date(outME$date, format="%Y-%m-%d")
outME <- subset(outME, date2>today)
outME$state<-"ME"
outME$democrat<-"Bellows"
outME$republican<-"Collins"
outME$lead<-ifelse(outME$who=="Bellows minus Collins","Democrat lead", "Republican lead")
outME$numdays <- electionday - today
outME$numpolls <- 0
write.csv(outME, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-massachusetts-senate-herr-vs-markey'
outMA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMA <- outMA[min(grep("minus",outMA$who)):nrow(outMA),]
outMA$date2 <- as.Date(outMA$date, format="%Y-%m-%d")
outMA <- subset(outMA, date2>today)
outMA$state<-"MA"
outMA$democrat<-"Markey"
outMA$republican<-"Herr"
outMA$lead<-ifelse(outMA$who=="Markey minus Herr","Democrat lead", "Republican lead")
outMA$numdays <- electionday - today
outMA$numpolls <- 0
write.csv(outMA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-michigan-senate-land-vs-peters'
outMI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMI <- outMI[min(grep("minus",outMI$who)):nrow(outMI),]
outMI$date2 <- as.Date(outMI$date, format="%Y-%m-%d")
outMI <- subset(outMI, date2>today)
outMI$state<-"MI"
outMI$democrat<-"Peters"
outMI$republican<-"Land"
outMI$lead<-ifelse(outMI$who=="Peters minus Land","Democrat lead", "Republican lead")
outMI$numdays <- electionday - today
outMI$numpolls <- 0
write.csv(outMI, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-minnesota-senate-mcfadden-vs-franken'
outMN <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMN <- outMN[min(grep("minus",outMN$who)):nrow(outMN),]
outMN$date2 <- as.Date(outMN$date, format="%Y-%m-%d")
outMN <- subset(outMN, date2>today)
outMN$state<-"MN"
outMN$democrat<-"Franken"
outMN$republican<-"McFadden"
outMN$lead<-ifelse(outMN$who=="Franken minus McFadden","Democrat lead", "Republican lead")
outMN$numdays <- electionday - today
outMN$numpolls <- 0
write.csv(outMN, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-mississippi-senate-cochran-vs-childers'
outMS <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMS <- outMS[min(grep("minus",outMS$who)):nrow(outMS),]
outMS$date2 <- as.Date(outMS$date, format="%Y-%m-%d")
outMS <- subset(outMS, date2>today)
outMS$state<-"MS"
outMS$democrat<-"Childers"
outMS$republican<-"Cochran"
outMS$lead<-ifelse(outMS$who=="Childers minus Cochran","Democrat lead", "Republican lead")
outMS$numdays <- electionday - today
outMS$numpolls <- 0
write.csv(outMS, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-montana-senate-daines-vs-curtis'
outMT <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outMT <- outMT[min(grep("minus",outMT$who)):nrow(outMT),]
outMT$date2 <- as.Date(outMT$date, format="%Y-%m-%d")
outMT <- subset(outMT, date2>today)
outMT$state<-"MT"
outMT$democrat<-"Curtis"
outMT$republican<-"Daines"
outMT$lead<-ifelse(outMT$who=="Curtis minus Daines","Democrat lead", "Republican lead")
outMT$numdays <- electionday - today
outMT$numpolls <- 0
write.csv(outMT, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-nebraska-senate-sasse-vs-domina'
outNE <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNE <- outNE[min(grep("minus",outNE$who)):nrow(outNE),]
outNE$date2 <- as.Date(outNE$date, format="%Y-%m-%d")
outNE <- subset(outNE, date2>today)
outNE$state<-"NE"
outNE$democrat<-"Domina"
outNE$republican<-"Sasse"
outNE$lead<-ifelse(outNE$who=="Domina minus Sasse","Democrat lead", "Republican lead")
outNE$numdays <- electionday - today
outNE$numpolls <- 0
write.csv(outNE, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-north-carolina-senate-tillis-vs-hagan'
outNC <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNC <- outNC[min(grep("minus",outNC$who)):nrow(outNC),]
outNC$date2 <- as.Date(outNC$date, format="%Y-%m-%d")
outNC <- subset(outNC, date2>today)
outNC$state<-"NC"
outNC$democrat<-"Hagan"
outNC$republican<-"Tillis"
outNC$lead<-ifelse(outNC$who=="Hagan minus Tillis","Democrat lead", "Republican lead")
outNC$numdays <- electionday - today
outNC$numpolls <- 0
write.csv(outNC, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-new-hampshire-senate-brown-vs-shaheen'
outNH <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNH <- outNH[min(grep("minus",outNH$who)):nrow(outNH),]
outNH$date2 <- as.Date(outNH$date, format="%Y-%m-%d")
outNH <- subset(outNH, date2>today)
outNH$state<-"NH"
outNH$democrat<-"Shaheen"
outNH$republican<-"Brown"
outNH$lead<-ifelse(outNH$who=="Shaheen minus Brown","Democrat lead", "Republican lead")
outNH$numdays <- electionday - today
outNH$numpolls <- 0
write.csv(outNH, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-new-jersey-senate-bell-vs-booker'
outNJ <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNJ <- outNJ[min(grep("minus",outNJ$who)):nrow(outNJ),]
outNJ$date2 <- as.Date(outNJ$date, format="%Y-%m-%d")
outNJ <- subset(outNJ, date2>today)
outNJ$state<-"NJ"
outNJ$democrat<-"Booker"
outNJ$republican<-"Bell"
outNJ$lead<-ifelse(outNJ$who=="Booker minus Bell","Democrat lead", "Republican lead")
outNJ$numdays <- electionday - today
outNJ$numpolls <- 0
write.csv(outNJ, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-new-mexico-senate-weh-vs-udall'
outNM <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outNM <- outNM[min(grep("minus",outNM$who)):nrow(outNM),]
outNM$date2 <- as.Date(outNM$date, format="%Y-%m-%d")
outNM <- subset(outNM, date2>today)
outNM$state<-"NM"
outNM$democrat<-"Udall"
outNM$republican<-"Weh"
outNM$lead<-ifelse(outNM$who=="Udall minus Weh","Democrat lead", "Republican lead")
outNM$numdays <- electionday - today
outNM$numpolls <- 0
write.csv(outNM, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-oklahoma-senate-inhofe-vs-silverstein'
outOK1 <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOK1 <- outOK1[min(grep("minus",outOK1$who)):nrow(outOK1),]
outOK1$date2 <- as.Date(outOK1$date, format="%Y-%m-%d")
outOK1 <- subset(outOK1, date2>today)
outOK1$state<-"OK1"
outOK1$democrat<-"Silverstein"
outOK1$republican<-"Inhofe"
outOK1$lead<-ifelse(outOK1$who=="Silverstein minus Inhofe","Democrat lead", "Republican lead")
outOK1$numdays <- electionday - today
outOK1$numpolls <- 0
write.csv(outOK1, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-oklahoma-senate-lankford-vs-johnson'
outOK2 <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOK2 <- outOK2[min(grep("minus",outOK2$who)):nrow(outOK2),]
outOK2$date2 <- as.Date(outOK2$date, format="%Y-%m-%d")
outOK2 <- subset(outOK2, date2>today)
outOK2$state<-"OK2"
outOK2$democrat<-"Johnson"
outOK2$republican<-"Lankford"
outOK2$lead<-ifelse(outOK2$who=="Johnson minus Lankford","Democrat lead", "Republican lead")
outOK2$numdays <- electionday - today
outOK2$numpolls <- 0
write.csv(outOK2, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-oregon-senate-wehby-vs-merkley'
outOR <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outOR <- outOR[min(grep("minus",outOR$who)):nrow(outOR),]
outOR$date2 <- as.Date(outOR$date, format="%Y-%m-%d")
outOR <- subset(outOR, date2>today)
outOR$state<-"OR"
outOR$democrat<-"Merkley"
outOR$republican<-"Wehby"
outOR$lead<-ifelse(outOR$who=="Merkley minus Wehby","Democrat lead", "Republican lead")
outOR$numdays <- electionday - today
outOR$numpolls <- 0
write.csv(outOR, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-rhode-island-senate-zaccaria-vs-reed'
outRI <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outRI <- outRI[min(grep("minus",outRI$who)):nrow(outRI),]
outRI$date2 <- as.Date(outRI$date, format="%Y-%m-%d")
outRI <- subset(outRI, date2>today)
outRI$state<-"RI"
outRI$democrat<-"Reed"
outRI$republican<-"Zaccaria"
outRI$lead<-ifelse(outRI$who=="Reed minus Zaccaria","Democrat lead", "Republican lead")
outRI$numdays <- electionday - today
outRI$numpolls <- 0
write.csv(outRI, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-senate-graham-vs-hutto'
outSC1 <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSC1 <- outSC1[min(grep("minus",outSC1$who)):nrow(outSC1),]
outSC1$date2 <- as.Date(outSC1$date, format="%Y-%m-%d")
outSC1 <- subset(outSC1, date2>today)
outSC1$state<-"SC1"
outSC1$democrat<-"Hutto"
outSC1$republican<-"Graham"
outSC1$lead<-ifelse(outSC1$who=="Hutto minus Graham","Democrat lead", "Republican lead")
outSC1$numdays <- electionday - today
outSC1$numpolls <- 0
write.csv(outSC1, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-senate-scott-vs-dickerson'
outSC2 <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSC2 <- outSC2[min(grep("minus",outSC2$who)):nrow(outSC2),]
outSC2$date2 <- as.Date(outSC2$date, format="%Y-%m-%d")
outSC2 <- subset(outSC2, date2>today)
outSC2$state<-"SC2"
outSC2$democrat<-"Dickerson"
outSC2$republican<-"Scott"
outSC2$lead<-ifelse(outSC2$who=="Dickerson minus Scott","Democrat lead", "Republican lead")
outSC2$numdays <- electionday - today
outSC2$numpolls <- 0
write.csv(outSC2, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-south-dakota-senate-rounds-vs-weiland'
outSD <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outSD <- outSD[min(grep("minus",outSD$who)):nrow(outSD),]
outSD$date2 <- as.Date(outSD$date, format="%Y-%m-%d")
outSD <- subset(outSD, date2>today)
outSD$state<-"SD"
outSD$democrat<-"Weiland"
outSD$republican<-"Rounds"
outSD$lead<-ifelse(outSD$who=="Weiland minus Rounds","Democrat lead", "Republican lead")
outSD$numdays <- electionday - today
outSD$numpolls <- 0
write.csv(outSD, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-tennessee-senate-alexander-vs-ball'
outTN <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outTN <- outTN[min(grep("minus",outTN$who)):nrow(outTN),]
outTN$date2 <- as.Date(outTN$date, format="%Y-%m-%d")
outTN <- subset(outTN, date2>today)
outTN$state<-"TN"
outTN$democrat<-"Ball"
outTN$republican<-"Alexander"
outTN$lead<-ifelse(outTN$who=="Ball minus Alexander","Democrat lead", "Republican lead")
outTN$numdays <- electionday - today
outTN$numpolls <- 0
write.csv(outTN, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-texas-senate-cornyn-vs-alameel'
outTX <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outTX <- outTX[min(grep("minus",outTX$who)):nrow(outTX),]
outTX$date2 <- as.Date(outTX$date, format="%Y-%m-%d")
outTX <- subset(outTX, date2>today)
outTX$state<-"TX"
outTX$democrat<-"Alameel"
outTX$republican<-"Cornyn"
outTX$lead<-ifelse(outTX$who=="Alameel minus Cornyn","Democrat lead", "Republican lead")
outTX$numdays <- electionday - today
outTX$numpolls <- 0
write.csv(outTX, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-virginia-senate-gillespie-vs-warner'
outVA <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outVA <- outVA[min(grep("minus",outVA$who)):nrow(outVA),]
outVA$date2 <- as.Date(outVA$date, format="%Y-%m-%d")
outVA <- subset(outVA, date2>today)
outVA$state<-"VA"
outVA$democrat<-"Warner"
outVA$republican<-"Gillespie"
outVA$lead<-ifelse(outVA$who=="Warner minus Gillespie","Democrat lead", "Republican lead")
outVA$numdays <- electionday - today
outVA$numpolls <- 0
write.csv(outVA, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-west-virginia-senate-capito-vs-tennant'
outWV <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outWV <- outWV[min(grep("minus",outWV$who)):nrow(outWV),]
outWV$date2 <- as.Date(outWV$date, format="%Y-%m-%d")
outWV <- subset(outWV, date2>today)
outWV$state<-"WV"
outWV$democrat<-"Tennant"
outWV$republican<-"Capito"
outWV$lead<-ifelse(outWV$who=="Tennant minus Capito","Democrat lead", "Republican lead")
outWV$numdays <- electionday - today
outWV$numpolls <- 0
write.csv(outWV, file=paste('post/',chart,'.csv',sep=''))

chart <- '2014-wyoming-senate'
outWY <- read.csv(paste(dataDir,chart,'/out.csv',sep=''))
outWY <- outWY[min(grep("minus",outWY$who)):nrow(outWY),]
outWY$date2 <- as.Date(outWY$date, format="%Y-%m-%d")
outWY <- subset(outWY, date2>today)
outWY$state<-"WY"
outWY$democrat<-"Hardy"
outWY$republican<-"Enzi"
outWY$lead<-ifelse(outWY$who=="Hardy minus Enzi","Democrat lead", "Republican lead")
outWY$numdays <- electionday - today
outWY$numpolls <- 0
write.csv(outWY, file=paste('post/',chart,'.csv',sep=''))

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
allstates$call[allstates$lead=="Independent lead" & allstates$prob2 >= 50] <- "I"
allstates$call[allstates$lead=="Independent lead" & allstates$prob2 < 50] <- "R"
allstates$call[allstates$lead=="Republican lead" & allstates$prob2 < 50 & allstates$state=="KS"] <- "I"

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

##Georgia probability
outGA <- read.csv(paste(dataDir,'2014-georgia-senate-perdue-vs-nunn/out.csv',sep=''))
outGA$date2 <- as.Date(outGA$date, format="%Y-%m-%d")
undecidedPct = subset(outGA, who=="Undecided" & date2==today)$xibar
outGAP <- subset(outGA, outGA$who=="Perdue")
outGAP <- subset(outGAP, date2==as.Date("2014-11-04"))
outGAP <- outGAP[,c("xibar", "up")]
outGAP$up <- outGAP$up + undecidedPct/2
outGAP$xibar <- outGAP$xibar + undecidedPct/2
PerdueSD <- ((outGAP$up - outGAP$xibar)/1.64)
PerdueZ <- (50.001 - outGAP$xibar)/PerdueSD
PerdueProb <- round((pnorm(-abs(PerdueZ))),3)
outGAN <- subset(outGA, outGA$who=="Nunn")
outGAN <- subset(outGAN, date2==as.Date("2014-11-04"))
outGAN <- outGAN[,c("xibar", "up")]
outGAN$up <- outGAN$up + undecidedPct/2
outGAN$xibar <- outGAN$xibar + undecidedPct/2
NunnSD <- ((outGAN$up - outGAN$xibar)/1.64)
NunnZ <- (50.001 - outGAN$xibar)/NunnSD
NunnProb <- round((pnorm(-abs(NunnZ))),3)
runoffprob <- 1 - (PerdueProb + NunnProb)

GAR <- subset(allstates, allstates$state=="GA-runoff")
PerdueRunoff <- 0
NunnRunoff <- 0
PerdueRunoff[GAR$call=="R"] <- GAR$finalprob/100
NunnRunoff[GAR$call=="R"] <- 1-PerdueRunoff
NunnRunoff[GAR$call=="D"] <- GAR$finalprob/100
PerdueRunoff[GAR$call=="D"] <- 1-NunnRunoff

finalPerdue <- (PerdueProb + (runoffprob * PerdueRunoff))*100
finalNunn <- (NunnProb + (runoffprob * NunnRunoff))*100

# Create row for primary
primary <- subset(allstates, state=="GA")
primary$state <- "GA-primary"
primary$finalprob <- subset(allstates, state=="GA")$finalprob
primary$pollprob <- subset(allstates, state=="GA")$pollprob
allstates <- rbind(allstates, primary)

allstates$finalprob[allstates$state=="GA" & allstates$call=="D"] <- finalNunn
allstates$finalprob[allstates$state=="GA" & allstates$call=="R"] <- finalPerdue

# Add column for runoff %
allstates$runoff <- ifelse(allstates$state=="GA-primary",runoffprob*100,0)

#write.csv(allstates,"allstates.csv") ##write the whole file only if you need to check everything

outSenate14 <- allstates[, c("state", "call", "finalprob", "pollprob", "democrat", "republican", "numdays", "runoff")]
write.csv(outSenate14, "post/outSenate14.csv")
write.csv(outSenate14, paste("post/outSenate14_",today,".csv",sep=""))
if (file.exists("/var/www/html/elections")) {
  write.csv(outSenate14, "/var/www/html/pollster/shared/models/outSenate14.csv")
  write.csv(outSenate14, paste("/var/www/html/pollster/shared/models/outSenate14_",today,".csv",sep=""))
  write.csv(outSenate14, "/var/www/html/elections/shared/senate_2014/outSenate14.csv")
  write.csv(outSenate14, paste("/var/www/html/elections/shared/senate_2014/outSenate14_",today,".csv",sep=""))
}

#convert everything over to R-side probs for R takeover prob calculation
allstates$RprobA <- 0
allstates$RprobA[allstates$call=="D"] <- 100 ##need to subtract from 100 for R prob
allstates$RprobA[allstates$call=="I"] <- 100
allstates$Rprob <- ifelse(allstates$RprobA==100, as.numeric(allstates$RprobA) - as.numeric(allstates$finalprob), allstates$finalprob)
allstates$Rprob <- as.numeric(allstates$Rprob)

# Monte Carlo approach for generating overall Republican win probability
if (!file.exists("/var/www/html/elections")) {
  sims <- 10                                    # number of simulations to run
  gopWins <- 0                                  # number of simulations in which GOP won a majority
  for (s in 1:sims) {                           # iterate over simulations
    gopSeats <- 0                               # number of GOP seats won in this simulation
    for (i in 1:nrow(allstates)) {              # iterate over races
      seat <- allstates[i, ]
      if (sample(0:99, 1) < seat['Rprob']) { # generate a random number between 0 and 99, if random number is less than GOP prob, increment GOP seats won
        gopSeats = gopSeats + 1
      }
    }
    if (gopSeats + 30 > 50) {                   # if GOP won the majority in this simulation, increment GOP wins
      gopWins = gopWins + 1
    }
  }
  print(paste("After ",sims," simulations, GOP won ",gopWins," (",(gopWins/sims*100),"%)", sep="")) # print percent of simulations where GOP won majority


  Rtakeover <- (gopWins/sims*100)
  countD <- sum(allstates$call=="D") + 34
  countR <- sum(allstates$call=="R") + 30
  countI <- sum(allstates$call=="I")

  SeatCount <- rbind(countD, countR, Rtakeover)

  write.csv(SeatCount, "post/seatcount.csv")
}

if (file.exists("/var/www/html/elections")) {
  system(paste("mkdir -p /var/www/html/pollster/shared/models/",today,"/post",sep=""))
  system(paste("cp post/2014-*.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
  system(paste("cp post/all*.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
  system(paste("cp post/pollstates.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
  system(paste("cp post/outSenate14.csv /var/www/html/pollster/shared/models/",today,"/post/",sep=""))
}
