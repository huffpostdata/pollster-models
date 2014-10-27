library(rjson)

dir.create('house/', showWarnings=FALSE, recursive=TRUE)
if (file.exists("/var/www/html/pollster")) {
  dataDir <- '/var/www/html/pollster/shared/models/'
} else {
  dataDir <- 'data/'
}

today <- as.Date(Sys.time(),tz="America/New_York")

##no polls states to be added as needed: AL, RI

chart <- '2014-alaska-senate-sullivan-vs-begich'
houseAK <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseAK <- houseAK[min(grep("minus",houseAK$who)):nrow(houseAK),]
houseAK$state<-"AK"
houseAK$democrat<-"Begich"
houseAK$republican<-"Sullivan"
houseAK$direction<-ifelse(houseAK$who=="Begich minus Sullivan","Democrat positive", "Republican positive")
write.csv(houseAK, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-arkansas-senate-cotton-vs-pryor'
houseAR <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseAR <- houseAR[min(grep("minus",houseAR$who)):nrow(houseAR),]
houseAR$state<-"AR"
houseAR$democrat<-"Pryor"
houseAR$republican<-"Cotton"
houseAR$direction<-ifelse(houseAR$who=="Pryor minus Cotton","Democrat positive", "Republican positive")
write.csv(houseAR, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-colorado-senate-gardner-vs-udall'
houseCO <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseCO <- houseCO[min(grep("minus",houseCO$who)):nrow(houseCO),]
houseCO$state<-"CO"
houseCO$democrat<-"Udall"
houseCO$republican<-"Gardner"
houseCO$direction<-ifelse(houseCO$who=="Udall minus Gardner","Democrat positive", "Republican positive")
write.csv(houseCO, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-delaware-senate-wade-vs-coons'
houseDE <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseDE <- houseDE[min(grep("minus",houseDE$who)):nrow(houseDE),]
houseDE$state<-"DE"
houseDE$democrat<-"Coons"
houseDE$republican<-"Wade"
houseDE$direction<-ifelse(houseDE$who=="Coons minus Wade","Democrat positive", "Republican positive")
write.csv(houseDE, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-georgia-senate-perdue-vs-nunn'
houseGA <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseGA <- houseGA[min(grep("minus",houseGA$who)):nrow(houseGA),]
houseGA$state<-"GA"
houseGA$democrat<-"Nunn"
houseGA$republican<-"Perdue"
houseGA$direction<-ifelse(houseGA$who=="Nunn minus Perdue","Democrat positive", "Republican positive")
write.csv(houseGA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-georgia-senate-runoff'
houseGAR <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseGAR <- houseGAR[min(grep("minus",houseGAR$who)):nrow(houseGAR),]
houseGAR$state<-"GAR"
houseGAR$democrat<-"Nunn"
houseGAR$republican<-"Perdue"
houseGAR$direction<-ifelse(houseGAR$who=="Nunn minus Perdue","Democrat positive", "Republican positive")
write.csv(houseGAR, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-hawaii-senate-cavasso-vs-schatz'
houseHI <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseHI <- houseHI[min(grep("minus",houseHI$who)):nrow(houseHI),]
houseHI$state<-"HI"
houseHI$democrat<-"Schatz"
houseHI$republican<-"Cavasso"
houseHI$direction<-ifelse(houseHI$who=="Schatz minus Cavasso","Democrat positive", "Republican positive")
write.csv(houseHI, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-iowa-senate-ernst-vs-braley'
houseIA <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseIA <- houseIA[min(grep("minus",houseIA$who)):nrow(houseIA),]
houseIA$state<-"IA"
houseIA$democrat<-"Braley"
houseIA$republican<-"Ernst"
houseIA$direction<-ifelse(houseIA$who=="Braley minus Ernst","Democrat positive", "Republican positive")
write.csv(houseIA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-idaho-senate-risch-vs-mitchell'
houseID <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseID <- houseID[min(grep("minus",houseID$who)):nrow(houseID),]
houseID$state<-"ID"
houseID$democrat<-"Mitchell"
houseID$republican<-"Risch"
houseID$direction<-ifelse(houseID$who=="Mitchell minus Risch","Democrat positive", "Republican positive")
write.csv(houseID, file=paste('house/',chart,'.csv',sep=''))


chart <- '2014-illinois-senate-oberweis-vs-durbin'
houseIL <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseIL <- houseIL[min(grep("minus",houseIL$who)):nrow(houseIL),]
houseIL$state<-"IL"
houseIL$democrat<-"Durbin"
houseIL$republican<-"Oberweis"
houseIL$direction<-ifelse(houseIL$who=="Durbin minus Oberweis","Democrat positive", "Republican positive")
write.csv(houseIL, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-kansas-senate-roberts-vs-orman-vs-taylor'
houseKS <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseKS <- houseKS[min(grep("minus",houseKS$who)):nrow(houseKS),]
houseKS$state<-"KS"
houseKS$democrat<-"Orman"
houseKS$republican<-"Roberts"
houseKS$direction<-ifelse(houseKS$who=="Orman minus Roberts","Democrat positive", "Republican positive")
write.csv(houseKS, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-kentucky-senate-mcconnell-vs-grimes'
houseKY <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseKY <- houseKY[min(grep("minus",houseKY$who)):nrow(houseKY),]
houseKY$state<-"KY"
houseKY$democrat<-"Grimes"
houseKY$republican<-"McConnell"
houseKY$direction<-ifelse(houseKY$who=="Grimes minus McConnell","Democrat positive", "Republican positive")
write.csv(houseKY, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-louisiana-senate-cassidy-vs-landrieu'
houseLA <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseLA <- houseLA[min(grep("minus",houseLA$who)):nrow(houseLA),]
houseLA$state<-"LA"
houseLA$democrat<-"Landrieu"
houseLA$republican<-"Cassidy"
houseLA$direction<-ifelse(houseLA$who=="Landrieu minus Cassidy","Democrat positive", "Republican positive")
write.csv(houseLA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-maine-senate-collins-vs-bellows'
houseME <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseME <- houseME[min(grep("minus",houseME$who)):nrow(houseME),]
houseME$state<-"ME"
houseME$democrat<-"Bellows"
houseME$republican<-"Collins"
houseME$direction<-ifelse(houseME$who=="Bellows minus Collins","Democrat positive", "Republican positive")
write.csv(houseME, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-massachusetts-senate-herr-vs-markey'
houseMA <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseMA <- houseMA[min(grep("minus",houseMA$who)):nrow(houseMA),]
houseMA$state<-"MA"
houseMA$democrat<-"Markey"
houseMA$republican<-"Herr"
houseMA$direction<-ifelse(houseMA$who=="Markey minus Herr","Democrat positive", "Republican positive")
write.csv(houseMA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-michigan-senate-land-vs-peters'
houseMI <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseMI <- houseMI[min(grep("minus",houseMI$who)):nrow(houseMI),]
houseMI$state<-"MI"
houseMI$democrat<-"Peters"
houseMI$republican<-"Land"
houseMI$direction<-ifelse(houseMI$who=="Peters minus Land","Democrat positive", "Republican positive")
write.csv(houseMI, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-minnesota-senate-mcfadden-vs-franken'
houseMN <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseMN <- houseMN[min(grep("minus",houseMN$who)):nrow(houseMN),]
houseMN$state<-"MN"
houseMN$democrat<-"Franken"
houseMN$republican<-"McFadden"
houseMN$direction<-ifelse(houseMN$who=="Franken minus McFadden","Democrat positive", "Republican positive")
write.csv(houseMN, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-mississippi-senate-cochran-vs-childers'
houseMS <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseMS <- houseMS[min(grep("minus",houseMS$who)):nrow(houseMS),]
houseMS$state<-"MS"
houseMS$democrat<-"Childers"
houseMS$republican<-"Cochran"
houseMS$direction<-ifelse(houseMS$who=="Childers minus Cochran","Democrat positive", "Republican positive")
write.csv(houseMS, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-montana-senate-daines-vs-curtis'
houseMT <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseMT <- houseMT[min(grep("minus",houseMT$who)):nrow(houseMT),]
houseMT$state<-"MT"
houseMT$democrat<-"Curtis"
houseMT$republican<-"Daines"
houseMT$direction<-ifelse(houseMT$who=="Curtis minus Daines","Democrat positive", "Republican positive")
write.csv(houseMT, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-nebraska-senate-sasse-vs-domina'
houseNE <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseNE <- houseNE[min(grep("minus",houseNE$who)):nrow(houseNE),]
houseNE$state<-"NE"
houseNE$democrat<-"Domina"
houseNE$republican<-"Sasse"
houseNE$direction<-ifelse(houseNE$who=="Domina minus Sasse","Democrat positive", "Republican positive")
write.csv(houseNE, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-north-carolina-senate-tillis-vs-hagan'
houseNC <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseNC <- houseNC[min(grep("minus",houseNC$who)):nrow(houseNC),]
houseNC$state<-"NC"
houseNC$democrat<-"Hagan"
houseNC$republican<-"Tillis"
houseNC$direction<-ifelse(houseNC$who=="Hagan minus Tillis","Democrat positive", "Republican positive")
write.csv(houseNC, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-new-hampshire-senate-brown-vs-shaheen'
houseNH <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseNH <- houseNH[min(grep("minus",houseNH$who)):nrow(houseNH),]
houseNH$state<-"NH"
houseNH$democrat<-"Shaheen"
houseNH$republican<-"Brown"
houseNH$direction<-ifelse(houseNH$who=="Shaheen minus Brown","Democrat positive", "Republican positive")
write.csv(houseNH, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-new-jersey-senate-bell-vs-booker'
houseNJ <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseNJ <- houseNJ[min(grep("minus",houseNJ$who)):nrow(houseNJ),]
houseNJ$state<-"NJ"
houseNJ$democrat<-"Booker"
houseNJ$republican<-"Bell"
houseNJ$direction<-ifelse(houseNJ$who=="Booker minus Bell","Democrat positive", "Republican positive")
write.csv(houseNJ, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-new-mexico-senate-weh-vs-udall'
houseNM <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseNM <- houseNM[min(grep("minus",houseNM$who)):nrow(houseNM),]
houseNM$state<-"NM"
houseNM$democrat<-"Udall"
houseNM$republican<-"Weh"
houseNM$direction<-ifelse(houseNM$who=="Udall minus Weh","Democrat positive", "Republican positive")
write.csv(houseNM, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-oklahoma-senate-inhofe-vs-silverstein'
houseOK1 <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseOK1 <- houseOK1[min(grep("minus",houseOK1$who)):nrow(houseOK1),]
houseOK1$state<-"OK1"
houseOK1$democrat<-"Silverstein"
houseOK1$republican<-"Inhofe"
houseOK1$direction<-ifelse(houseOK1$who=="Silverstein minus Inhofe","Democrat positive", "Republican positive")
write.csv(houseOK1, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-oklahoma-senate-lankford-vs-johnson'
houseOK2 <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseOK2 <- houseOK2[min(grep("minus",houseOK2$who)):nrow(houseOK2),]
houseOK2$state<-"OK2"
houseOK2$democrat<-"Johnson"
houseOK2$republican<-"Lankford"
houseOK2$direction<-ifelse(houseOK2$who=="Johnson minus Lankford","Democrat positive", "Republican positive")
write.csv(houseOK2, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-oregon-senate-wehby-vs-merkley'
houseOR <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseOR <- houseOR[min(grep("minus",houseOR$who)):nrow(houseOR),]
houseOR$state<-"OR"
houseOR$democrat<-"Merkley"
houseOR$republican<-"Wehby"
houseOR$direction<-ifelse(houseOR$who=="Merkley minus Wehby","Democrat positive", "Republican positive")
write.csv(houseOR, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-rhode-island-senate-zaccaria-vs-reed'
houseRI <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseRI <- houseRI[min(grep("minus",houseRI$who)):nrow(houseRI),]
houseRI$state<-"RI"
houseRI$democrat<-"Reed"
houseRI$republican<-"Zaccaria"
houseRI$direction<-ifelse(houseRI$who=="Reed minus Zaccaria","Democrat positive", "Republican positive")
write.csv(houseRI, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-senate-graham-vs-hutto'
houseSC1 <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseSC1 <- houseSC1[min(grep("minus",houseSC1$who)):nrow(houseSC1),]
houseSC1$state<-"SC1"
houseSC1$democrat<-"Hutto"
houseSC1$republican<-"Graham"
houseSC1$direction<-ifelse(houseSC1$who=="Hutto minus Graham","Democrat positive", "Republican positive")
write.csv(houseSC1, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-senate-scott-vs-dickerson'
houseSC2 <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseSC2 <- houseSC2[min(grep("minus",houseSC2$who)):nrow(houseSC2),]
houseSC2$state<-"SC2"
houseSC2$democrat<-"Dickerson"
houseSC2$republican<-"Scott"
houseSC2$direction<-ifelse(houseSC2$who=="Dickerson minus Scott","Democrat positive", "Republican positive")
write.csv(houseSC2, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-south-dakota-senate-rounds-vs-weiland'
houseSD <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseSD <- houseSD[min(grep("minus",houseSD$who)):nrow(houseSD),]
houseSD$state<-"SD"
houseSD$democrat<-"Weiland"
houseSD$republican<-"Rounds"
houseSD$direction<-ifelse(houseSD$who=="Weiland minus Rounds","Democrat positive", "Republican positive")
write.csv(houseSD, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-tennessee-senate-alexander-vs-ball'
houseTN <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseTN <- houseTN[min(grep("minus",houseTN$who)):nrow(houseTN),]
houseTN$state<-"TN"
houseTN$democrat<-"Ball"
houseTN$republican<-"Alexander"
houseTN$direction<-ifelse(houseTN$who=="Begich minus Sullivan","Democrat positive", "Republican positive")
write.csv(houseTN, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-texas-senate-cornyn-vs-alameel'
houseTX <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseTX <- houseTX[min(grep("minus",houseTX$who)):nrow(houseTX),]
houseTX$state<-"TX"
houseTX$democrat<-"Alameel"
houseTX$republican<-"Cornyn"
houseTX$direction<-ifelse(houseTX$who=="Alameel minus Cornyn","Democrat positive", "Republican positive")
write.csv(houseTX, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-virginia-senate-gillespie-vs-warner'
houseVA <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseVA <- houseVA[min(grep("minus",houseVA$who)):nrow(houseVA),]
houseVA$state<-"VA"
houseVA$democrat<-"Warner"
houseVA$republican<-"Gillespie"
houseVA$direction<-ifelse(houseVA$who=="Warner minus Gillespie","Democrat positive", "Republican positive")
write.csv(houseVA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-west-virginia-senate-capito-vs-tennant'
houseWV <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseWV <- houseWV[min(grep("minus",houseWV$who)):nrow(houseWV),]
houseWV$state<-"WV"
houseWV$democrat<-"Tennant"
houseWV$republican<-"Capito"
houseWV$direction<-ifelse(houseWV$who=="Tennant minus Capito","Democrat positive", "Republican positive")
write.csv(houseWV, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-wyoming-senate'
houseWY <- read.csv(paste(dataDir,chart,'/nathouse.csv',sep=''))
houseWY <- houseWY[min(grep("minus",houseWY$who)):nrow(houseWY),]
houseWY$state<-"WY"
houseWY$democrat<-"Hardy"
houseWY$republican<-"Enzi"
houseWY$direction<-ifelse(houseWY$who=="Hardy minus Enzi","Democrat positive", "Republican positive")
write.csv(houseWY, file=paste('house/',chart,'.csv',sep=''))


filenames <- list.files(path="house/", pattern='2014.*csv', full.names=TRUE)
allhouse<-do.call("rbind", lapply(filenames, read.csv, header=TRUE))
allhouse$X.1<-NULL ##deletes defunct case number column
allhouse$X<-NULL  ##deletes the other defunct case number column
allhouse$direction2[allhouse$direction=="Democrat positive"] <- "-1"
allhouse$direction2[allhouse$direction=="Republican positive"] <- "1"
allhouse$direction2 <- as.numeric(allhouse$direction2)
allhouse$est2 <- (allhouse$direction2 * allhouse$est)
allhouse$est2 <- round(allhouse$est2, 2)
allhouse$lo2 <- ifelse(allhouse$direction2 == 1, allhouse$lo, allhouse$direction2 * allhouse$hi)
allhouse$lo2 <- round(allhouse$lo2, 2)
allhouse$hi2 <- ifelse(allhouse$direction2 == 1, allhouse$hi, allhouse$direction2 * allhouse$lo)
allhouse$hi2 <- round(allhouse$hi2, 2)

write.csv(allhouse,"house/allhouse.csv") ##check to make sure everything s right


houseSenate14 <- allhouse[, c("state", "pollster", "est2", "lo2", "hi2", "dev")]

write.csv(houseSenate14, "house/houseSenate14.csv") ##may want to put this in a separate location from the other files.
if (file.exists("/var/www/html/elections")) {
  write.csv(houseSenate14, "/var/www/html/pollster/shared/models/houseSenate14.csv")
  write.csv(houseSenate14, paste("/var/www/html/pollster/shared/models/houseSenate14_",today,".csv",sep=""))
  write.csv(houseSenate14, "/var/www/html/elections/shared/senate_2014/houseSenate14.csv")
  write.csv(houseSenate14, paste("/var/www/html/elections/shared/senate_2014/houseSenate14_",today,".csv",sep=""))
}

if (file.exists("/var/www/html/elections")) {
  system(paste("mkdir -p /var/www/html/pollster/shared/models/",today,"/house",sep=""))
  system(paste("cp house/2014-*.csv /var/www/html/pollster/shared/models/",today,"/house/",sep=""))
  system(paste("cp house/allhouse.csv /var/www/html/pollster/shared/models/",today,"/house/",sep=""))
  system(paste("cp house/houseSenate14.csv /var/www/html/pollster/shared/models/",today,"/house/",sep=""))
}
