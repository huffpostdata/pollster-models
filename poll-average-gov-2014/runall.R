charts <- c(
  '2014-alaska-governor-parnell-vs-walker',
  '2014-arizona-governor-ducey-vs-duval',
  '2014-arkansas-governor-hutchinson-vs-ross',
  '2014-california-governor-kashkari-vs-brown',
  '2014-colorado-governor-beauprez-vs-hickenlooper',
  '2014-connecticut-governor-foley-vs-malloy',
  '2014-florida-governor-scott-vs-crist',
  '2014-georgia-governor-deal-vs-carter',
  '2014-hawaii-governor-aiona-vs-ige',
  '2014-idaho-governor-otter-vs-balukoff',
  '2014-illinois-governor-rauner-vs-quinn',
  '2014-iowa-governor-branstad-vs-hatch',
  '2014-kansas-governor-brownback-vs-davis',
  '2014-maine-governor-lepage-vs-michaud-vs-cutler',
  '2014-maryland-governor-hogan-vs-brown',
  '2014-massachusetts-governor-baker-vs-coakley',
  '2014-michigan-governor-snyder-vs-schauer',
  '2014-minnesota-governor-dayton-vs-johnson',
  '2014-nebraska-governor-ricketts-vs-hassebrook',
  '2014-nevada-governor-sandoval-vs-goodman',
  '2014-new-hampshire-governor-havenstein-vs-hassan',
  '2014-new-mexico-governor-martinez-vs-king',
  '2014-new-york-governor-astorino-vs-cuomo',
  '2014-ohio-governor-kasich-vs-fitzgerald',
  '2014-oklahoma-governor-fallin-vs-dorman',
  '2014-oregon-governor-richardson-vs-kitzhaber',
  '2014-pennsylvania-governor-corbett-vs-wolf',
  '2014-rhode-island-governor-fung-vs-raimondo'
  '2014-south-carolina-governor-haley-vs-sheheen',
  '2014-south-dakota-governor-daugaard-vs-wismer'
  '2014-texas-governor-abbott-vs-davis',
  '2014-wisconsin-governor-walker-vs-burke'
)

for (c in charts) {
  system(paste("Rscript poll-average-gov-2014.R '",c,"'",sep=''))
}

system("Rscript postprocessing.R")
system("Rscript house-effects.R")

