##no polls states to be added as needed: AL, HI, ID, NE, NV, OK, RI, SD, TN, VT, WY

library(rjson)

dir.create('house/', showWarnings=FALSE, recursive=TRUE)
if (file.exists("/var/www/html/pollster")) {
  dataDir <- '/var/www/html/pollster/shared/models/'
} else {
  dataDir <- 'data/'
}

today <- as.Date(Sys.time(),tz="America/New_York")

chart <- '2014-alaska-governor-parnell-vs-walker'
houseAK <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseAK <- houseAK[min(grep("minus",houseAK$who)):nrow(houseAK),] #this tells it to only import the "minus" data--which has the probability associated
houseAK$state<-"AK"
houseAK$democrat<-"Walker"
houseAK$republican<-"Parnell"
houseAK$direction<-ifelse(houseAK$who=="Walker minus Parnell","Independent positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseAK, file=paste('house/',chart,'.csv',sep='')) ##save file for merging later

chart <- '2014-arizona-governor-ducey-vs-duval'
houseAZ <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseAZ <- houseAZ[min(grep("minus",houseAZ$who)):nrow(houseAZ),] #this tells it to only import the "minus" data--which has the probability associated
houseAZ$state<-"AZ"
houseAZ$democrat<-"DuVal"
houseAZ$republican<-"Ducey"
houseAZ$direction<-ifelse(houseAZ$who=="DuVal minus Ducey","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseAZ, file=paste('house/',chart,'.csv',sep='')) ##save file for merging later

chart <- '2014-arkansas-governor-hutchinson-vs-ross'
houseAR <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseAR <- houseAR[min(grep("minus",houseAR$who)):nrow(houseAR),] #this tells it to only import the "minus" data--which has the probability associated
houseAR$state<-"AR"
houseAR$democrat<- "Ross"
houseAR$republican<-"Hutchinson"
houseAR$direction<-ifelse(houseAR$who=="Ross minus Hutchinson","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseAR, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-california-governor-kashkari-vs-brown'
houseCA <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseCA <- houseCA[min(grep("minus",houseCA$who)):nrow(houseCA),] #this tells it to only import the "minus" data--which has the probability associated
houseCA$state<-"CA"
houseCA$democrat<- "Brown"
houseCA$republican<-"Kashkari"
houseCA$direction<-ifelse(houseCA$who=="Brown minus Kashkari","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseCA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-colorado-governor-beauprez-vs-hickenlooper'
houseCO <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseCO <- houseCO[min(grep("minus",houseCO$who)):nrow(houseCO),] #this tells it to only import the "minus" data--which has the probability associated
houseCO$state<-"CO"
houseCO$democrat<- "Hickenlooper"
houseCO$republican<-"Beauprez"
houseCO$direction<-ifelse(houseCO$who=="Hickenlooper minus Beauprez","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseCO, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-connecticut-governor-foley-vs-malloy'
houseCT <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseCT <- houseCT[min(grep("minus",houseCT$who)):nrow(houseCT),] #this tells it to only import the "minus" data--which has the probability associated
houseCT$state<-"CT"
houseCT$democrat<- "Malloy"
houseCT$republican<-"Foley"
houseCT$direction<-ifelse(houseCT$who=="Malloy minus Foley","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseCT, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-florida-governor-scott-vs-crist'
houseFL <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseFL <- houseFL[min(grep("minus",houseFL$who)):nrow(houseFL),] #this tells it to only import the "minus" data--which has the probability associated
houseFL$state<-"FL"
houseFL$democrat<- "Crist"
houseFL$republican<-"Scott"
houseFL$direction<-ifelse(houseFL$who=="Crist minus Scott","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseFL, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-georgia-governor-deal-vs-carter'
houseGA <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseGA <- houseGA[min(grep("minus",houseGA$who)):nrow(houseGA),] #this tells it to only import the "minus" data--which has the probability associated
houseGA$state<-"GA"
houseGA$democrat<- "Carter"
houseGA$republican<-"Deal"
houseGA$direction<-ifelse(houseGA$who=="Carter minus Deal","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseGA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-illinois-governor-rauner-vs-quinn'
houseIL <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseIL <- houseIL[min(grep("minus",houseIL$who)):nrow(houseIL),] #this tells it to only import the "minus" data--which has the probability associated
houseIL$state<-"IL"
houseIL$democrat<- "Quinn"
houseIL$republican<-"Rauner"
houseIL$direction<-ifelse(houseIL$who=="Quinn minus Rauner","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseIL, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-iowa-governor-branstad-vs-hatch'
houseIA <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseIA <- houseIA[min(grep("minus",houseIA$who)):nrow(houseIA),] #this tells it to only import the "minus" data--which has the probability associated
houseIA$state<-"IA"
houseIA$democrat<- "Hatch"
houseIA$republican<-"Branstad"
houseIA$direction<-ifelse(houseIA$who=="Hatch minus Branstad","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseIA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-kansas-governor-brownback-vs-davis'
houseKS <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseKS <- houseKS[min(grep("minus",houseKS$who)):nrow(houseKS),] #this tells it to only import the "minus" data--which has the probability associated
houseKS$state<-"KS"
houseKS$democrat<- "Davis"
houseKS$republican<-"Brownback"
houseKS$direction<-ifelse(houseKS$who=="Davis minus Brownback","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseKS, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-maine-governor-lepage-vs-michaud-vs-cutler'
houseME <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseME <- houseME[min(grep("minus",houseME$who)):nrow(houseME),] #this tells it to only import the "minus" data--which has the probability associated
houseME$state<-"ME"
houseME$democrat<- "Michaud"
houseME$republican<-"LePage"
houseME$direction<-ifelse(houseME$who=="Michaud minus LePage","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseME, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-maryland-governor-hogan-vs-brown'
houseMD <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseMD <- houseMD[min(grep("minus",houseMD$who)):nrow(houseMD),] #this tells it to only import the "minus" data--which has the probability associated
houseMD$state<-"MD"
houseMD$democrat<- "Brown"
houseMD$republican<-"Hogan"
houseMD$direction<-ifelse(houseMD$who=="Brown minus Hogan","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseMD, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-massachusetts-governor-baker-vs-coakley'
houseMA <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseMA <- houseMA[min(grep("minus",houseMA$who)):nrow(houseMA),] #this tells it to only import the "minus" data--which has the probability associated
houseMA$state<-"MA"
houseMA$democrat<- "Coakley"
houseMA$republican<-"Baker"
houseMA$direction<-ifelse(houseMA$who=="Coakley minus Baker","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseMA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-michigan-governor-snyder-vs-schauer'
houseMI <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseMI <- houseMI[min(grep("minus",houseMI$who)):nrow(houseMI),] #this tells it to only import the "minus" data--which has the probability associated
houseMI$state<-"MI"
houseMI$democrat<- "Schauer"
houseMI$republican<-"Snyder"
houseMI$direction<-ifelse(houseMI$who=="Schauer minus Snyder","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseMI, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-minnesota-governor-dayton-vs-johnson'
houseMN <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseMN <- houseMN[min(grep("minus",houseMN$who)):nrow(houseMN),] #this tells it to only import the "minus" data--which has the probability associated
houseMN$state<-"MN"
houseMN$democrat<- "Dayton"
houseMN$republican<-"Johnson"
houseMN$direction<-ifelse(houseMN$who=="Dayton minus Johnson","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseMN, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-nebraska-governor-ricketts-vs-hassebrook'
houseNE <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseNE <- houseNE[min(grep("minus",houseNE$who)):nrow(houseNE),] #this tells it to only import the "minus" data--which has the probability associated
houseNE$state<-"NE"
houseNE$democrat<- "Hassebrook"
houseNE$republican<-"Ricketts"
houseNE$direction<-ifelse(houseNE$who=="Hassebrook minus Ricketts","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseNE, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-new-hampshire-governor-havenstein-vs-hassan'
houseNH <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseNH <- houseNH[min(grep("minus",houseNH$who)):nrow(houseNH),] #this tells it to only import the "minus" data--which has the probability associated
houseNH$state<-"NH"
houseNH$democrat<- "Hassan"
houseNH$republican<-"Havenstein"
houseNH$direction<-ifelse(houseNH$who=="Hassan minus Havenstein","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseNH, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-new-mexico-governor-martinez-vs-king'
houseNM <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseNM <- houseNM[min(grep("minus",houseNM$who)):nrow(houseNM),] #this tells it to only import the "minus" data--which has the probability associated
houseNM$state<-"NM"
houseNM$democrat<- "King"
houseNM$republican<-"Martinez"
houseNM$direction<-ifelse(houseNM$who=="King minus Martinez","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseNM, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-nevada-governor-sandoval-vs-goodman'
houseNV <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseNV <- houseNV[min(grep("minus",houseNV$who)):nrow(houseNV),] #this tells it to only import the "minus" data--which has the probability associated
houseNV$state<-"NV"
houseNV$democrat<- "Goodman"
houseNV$republican<-"Sandoval"
houseNV$direction<-ifelse(houseNV$who=="Goodman minus Sandoval","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseNV, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-new-york-governor-astorino-vs-cuomo'
houseNY <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseNY <- houseNY[min(grep("minus",houseNY$who)):nrow(houseNY),] #this tells it to only import the "minus" data--which has the probability associated
houseNY$state<-"NY"
houseNY$democrat<- "Cuomo"
houseNY$republican<-"Astorino"
houseNY$direction<-ifelse(houseNY$who=="Cuomo minus Astorino","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseNY, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-ohio-governor-kasich-vs-fitzgerald'
houseOH <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseOH <- houseOH[min(grep("minus",houseOH$who)):nrow(houseOH),] #this tells it to only import the "minus" data--which has the probability associated
houseOH$state<-"OH"
houseOH$democrat<- "Fitzgerald"
houseOH$republican<-"Kasich"
houseOH$direction<-ifelse(houseOH$who=="Fitzgerald minus Kasich","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseOH, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-oklahoma-governor-fallin-vs-dorman'
houseOK <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseOK <- houseOK[min(grep("minus",houseOK$who)):nrow(houseOK),] #this tells it to only import the "minus" data--which has the probability associated
houseOK$state<-"OK"
houseOK$democrat<- "Dorman"
houseOK$republican<-"Fallin"
houseOK$direction<-ifelse(houseOK$who=="Dorman minus Fallin","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseOK, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-oregon-governor-richardson-vs-kitzhaber'
houseOR <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseOR <- houseOR[min(grep("minus",houseOR$who)):nrow(houseOR),] #this tells it to only import the "minus" data--which has the probability associated
houseOR$state<-"OR"
houseOR$democrat<- "Kitzhaber"
houseOR$republican<-"Richardson"
houseOR$direction<-ifelse(houseOR$who=="Kitzhaber minus Richardson","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseOR, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-pennsylvania-governor-corbett-vs-wolf'
housePA <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
housePA <- housePA[min(grep("minus",housePA$who)):nrow(housePA),] #this tells it to only import the "minus" data--which has the probability associated
housePA$state<-"PA"
housePA$democrat<- "Wolf"
housePA$republican<-"Corbett"
housePA$direction<-ifelse(housePA$who=="Wolf minus Corbett","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(housePA, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-south-carolina-governor-haley-vs-sheheen'
houseSC <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseSC <- houseSC[min(grep("minus",houseSC$who)):nrow(houseSC),] #this tells it to only import the "minus" data--which has the probability associated
houseSC$state<-"SC"
houseSC$democrat<- "Sheheen"
houseSC$republican<-"Haley"
houseSC$direction<-ifelse(houseSC$who=="Sheheen minus Haley","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseSC, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-south-dakota-governor-daugaard-vs-wismer'
houseSD <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseSD <- houseSD[min(grep("minus",houseSD$who)):nrow(houseSD),] #this tells it to only import the "minus" data--which has the probability associated
houseSD$state<-"SD"
houseSD$democrat<- "Wismer"
houseSD$republican<-"Daugaard"
houseSD$direction<-ifelse(houseSD$who=="Wismer minus Daugaard","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseSD, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-texas-governor-abbott-vs-davis'
houseTX <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseTX <- houseTX[min(grep("minus",houseTX$who)):nrow(houseTX),] #this tells it to only import the "minus" data--which has the probability associated
houseTX$state<-"TX"
houseTX$democrat<- "Davis"
houseTX$republican<-"Abbott"
houseTX$direction<-ifelse(houseTX$who=="Davis minus Abbott","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseTX, file=paste('house/',chart,'.csv',sep=''))

chart <- '2014-wisconsin-governor-walker-vs-burke'
houseWI <- read.csv(paste(dataDir,chart,'/house.csv',sep=''))
houseWI <- houseWI[min(grep("minus",houseWI$who)):nrow(houseWI),] #this tells it to only import the "minus" data--which has the probability associated
houseWI$state<-"WI"
houseWI$democrat<- "Burke"
houseWI$republican<-"Walker"
houseWI$direction<-ifelse(houseWI$who=="Burke minus Walker","Democrat positive", "Republican positive") ##code whether probability shows Dem positive or Rep positive
write.csv(houseWI, file=paste('house/',chart,'.csv',sep=''))


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

houseGov14 <- allhouse[, c("state", "pollster", "est2", "lo2", "hi2", "dev")]

write.csv(houseGov14, "house/houseGov14.csv") ##may want to put this in a separate location from the other files.
if (file.exists("/var/www/html/elections")) {
  write.csv(houseGov14, "/var/www/html/pollster/shared/models/houseGov14.csv")
  write.csv(houseGov14, paste("/var/www/html/pollster/shared/models/houseGov14_",today,".csv",sep=""))
  write.csv(houseGov14, "/var/www/html/elections/shared/gov_2014/houseGov14.csv")
  write.csv(houseGov14, paste("/var/www/html/elections/shared/gov_2014/houseGov14_",today,".csv",sep=""))
}

if (file.exists("/var/www/html/elections")) {
  system(paste("mkdir -p /var/www/html/pollster/shared/models/",today,"/house",sep=""))
  system(paste("cp house/2014-*.csv /var/www/html/pollster/shared/models/",today,"/house/",sep=""))
  system(paste("cp house/allhouse.csv /var/www/html/pollster/shared/models/",today,"/house/",sep=""))
  system(paste("cp house/houseGov14.csv /var/www/html/pollster/shared/models/",today,"/house/",sep=""))
}