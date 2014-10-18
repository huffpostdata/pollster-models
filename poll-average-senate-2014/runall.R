charts <- c(
  '2014-alaska-senate-sullivan-vs-begich',
  '2014-arkansas-senate-cotton-vs-pryor',
  '2014-colorado-senate-gardner-vs-udall',
  '2014-georgia-senate-perdue-vs-nunn',
  '2014-hawaii-senate-cavasso-vs-schatz',
  '2014-illinois-senate-oberweis-vs-durbin',
  '2014-iowa-senate-ernst-vs-braley',
  '2014-idaho-senate-risch-vs-mitchell',
  '2014-kansas-senate-roberts-vs-orman-vs-taylor',
  '2014-kentucky-senate-mcconnell-vs-grimes',
  '2014-louisiana-senate-cassidy-vs-landrieu',
  '2014-maine-senate-collins-vs-bellows',
  '2014-massachusetts-senate-herr-vs-markey',
  '2014-michigan-senate-land-vs-peters',
  '2014-minnesota-senate-mcfadden-vs-franken',
  '2014-mississippi-senate-cochran-vs-childers',
  '2014-nebraska-senate-sasse-vs-domina',
  '2014-north-carolina-senate-tillis-vs-hagan',
  '2014-new-hampshire-senate-brown-vs-shaheen',
  '2014-new-jersey-senate-bell-vs-booker',
  '2014-new-mexico-senate-weh-vs-udall',
  '2014-oregon-senate-wehby-vs-merkley',
  '2014-south-carolina-senate-graham-vs-hutto',
  '2014-south-carolina-senate-scott-vs-dickerson',
  '2014-south-dakota-senate-rounds-vs-weiland',
  '2014-tennessee-senate-alexander-vs-ball',
  '2014-texas-senate-cornyn-vs-alameel',
  '2014-virginia-senate-gillespie-vs-warner',
  '2014-west-virginia-senate-capito-vs-tennant'
)

for (c in charts) {
  system(paste("Rscript poll-average-senate-2014.R '",c,"'",sep=''))
}

system("Rscript postprocessing.R")
system("Rscript house-effects.R")

