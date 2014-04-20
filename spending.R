# spending per pupil 2011
# http://www.census.gov/govs/school/
options(scipen=100)
require(data.table)

rm(list=ls(all=TRUE))
#individualUnitUrl <- "http://www2.census.gov/govs/school/elsec11t.txt"
#allDataItemsUrl   <- "http://www2.census.gov/govs/school/elsec11.txt"
#stateCountyUrl    <- "http://www.census.gov/geo/reference/codes/files/national_county.txt"
#download.file(individualUnitUrl,destfile="./data/elsec11t.txt",method="curl")
#download.file(allDataItemsUrl,destfile="./data/elsec11.txt",method="curl")
#download.file(stateCountyUrl,destfile="./data/national_county.txt",method="curl")

# individual unit table, comma-delimited; no issues
iuData <- data.table(read.table("./data/elsec11t.txt",sep=",",header=TRUE,row.names = NULL))

# all data items table, ditto; some screw-up in header of original txt file
adiData <- data.table(read.table("./data/elsec11.txt",sep=",",header=TRUE,row.names = NULL))
shiftNames <- names(adiData)
setnames(adiData,shiftNames,c(shiftNames[-1],'drop'))
adiData <- subset(adiData,select=-drop)

# labels for variable names
iuLabels <- read.table("./docs/individual_unit_labels",sep=",",header=TRUE,row.names = NULL)
adiLabels <- read.table("./docs/all_data_items_labels",sep=",",header=TRUE,row.names = NULL)
adiLabels$Data.Item <- gsub('^_','X_',adiLabels$Data.Item)

# States and counties
# had to copy-paste to Vim, because the asterisk in the name of
# Manu'a District, in American Samoa, messed up line ends forever after.
setStateCounty <- function(df) {
   conum <- data.table(read.table("./docs/national_county",sep=",",header=TRUE,row.names = NULL))
   conum <- subset(conum, select=-ANSI.Cl)
   df[,State.ANSI:=as.integer(substr(formatC(df[['CONUM']], width = 5, format = "d", flag = "0"),1,2))]
   df[,County.ANSI:=as.integer(substr(formatC(df[['CONUM']], width = 5, format = "d", flag = "0"),3,5))]
   out <- merge(conum,df,by=c('State.ANSI','County.ANSI'))
   return(subset(out,select=-CONUM))
}
iuStates <- setStateCounty(iuData)
adiStates <- setStateCounty(adiData)
