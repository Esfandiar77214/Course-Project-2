#Loading Required Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
#Loading Data
StormDataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
StormDataFile <- "data/storm-data.csv.bz2"

if (!file.exists('data')) {
  dir.create('data')
}

if (!file.exists(StormDataFile)) {
  download.file(url = StormDataURL, destfile = StormDataFile)
}

#Load the Dataset
StormData <- read.csv(StormDataFile, sep = ",", header = TRUE)

#Show the Structure of the Dataset
str(StormData)

#Check the First 6 rows
head(StormData)

StormDataTidy <- subset(StormData, EVTYPE !="?"
                        & (FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG >0),
                        select = c("EVTYPE",
                                   "FATALITIES",
                                   "INJURIES",
                                   "PROPDMG",
                                   "PROPDMGEXP",
                                   "CROPDMG",
                                   "CROPDMGEXP",
                                   "BGN_DATE",
                                   "END_DATE",
                                   "STATE"))

dim(StormDataTidy)
#Tidy Up Event Type
length(unique(StormDataTidy$EVTYPE))
StormDataTidy$EVTYPE <- toupper(StormDataTidy$EVTYPE)

#AVALANCHE
StormDataTidy$EVTYPE <- gsub('.*AVALANCE.*', 'AVALANCHE', StormDataTidy$EVTYPE)

# BLIZZARD
StormDataTidy$EVTYPE <- gsub('.*BLIZZARD.*', 'BLIZZARD', StormDataTidy$EVTYPE)

# CLOUD
StormDataTidy$EVTYPE <- gsub('.*CLOUD.*', 'CLOUD', StormDataTidy$EVTYPE)

# COLD
StormDataTidy$EVTYPE <- gsub('.*COLD.*', 'COLD', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*FREEZ.*', 'COLD', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*FROST.*', 'COLD', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*ICE.*', 'COLD', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*LOW TEMPERATURE RECORD.*', 'COLD', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*LO.*TEMP.*', 'COLD', StormDataTidy$EVTYPE)

# DRY
StormDataTidy$EVTYPE <- gsub('.*DRY.*', 'DRY', StormDataTidy$EVTYPE)

# DUST
StormDataTidy$EVTYPE <- gsub('.*DUST.*', 'DUST', StormDataTidy$EVTYPE)

# FIRE
StormDataTidy$EVTYPE <- gsub('.*FIRE.*', 'FIRE', StormDataTidy$EVTYPE)

# FLOOD
StormDataTidy$EVTYPE <- gsub('.*FLOOD.*', 'FLOOD', StormDataTidy$EVTYPE)

# FOG
StormDataTidy$EVTYPE <- gsub('.*FOG.*', 'FOG', StormDataTidy$EVTYPE)

# HAIL
StormDataTidy$EVTYPE <- gsub('.*HAIL.*', 'HAIL', StormDataTidy$EVTYPE)

# HEAT
StormDataTidy$EVTYPE <- gsub('.*HEAT.*', 'HEAT', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*WARM.*', 'HEAT', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*HIGH.*TEMP.*', 'HEAT', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*RECORD HIGH TEMPERATURES.*', 'HEAT', StormDataTidy$EVTYPE)

# HYPOTHERMIA/EXPOSURE
StormDataTidy$EVTYPE <- gsub('.*HYPOTHERMIA.*', 'HYPOTHERMIA/EXPOSURE', StormDataTidy$EVTYPE)

# LANDSLIDE
StormDataTidy$EVTYPE <- gsub('.*LANDSLIDE.*', 'LANDSLIDE', StormDataTidy$EVTYPE)

# LIGHTNING
StormDataTidy$EVTYPE <- gsub('^LIGHTNING.*', 'LIGHTNING', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('^LIGNTNING.*', 'LIGHTNING', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('^LIGHTING.*', 'LIGHTNING', StormDataTidy$EVTYPE)

# MICROBURST
StormDataTidy$EVTYPE <- gsub('.*MICROBURST.*', 'MICROBURST', StormDataTidy$EVTYPE)

# MUDSLIDE
StormDataTidy$EVTYPE <- gsub('.*MUDSLIDE.*', 'MUDSLIDE', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*MUD SLIDE.*', 'MUDSLIDE', StormDataTidy$EVTYPE)

# RAIN
StormDataTidy$EVTYPE <- gsub('.*RAIN.*', 'RAIN', StormDataTidy$EVTYPE)

# RIP CURRENT
StormDataTidy$EVTYPE <- gsub('.*RIP CURRENT.*', 'RIP CURRENT', StormDataTidy$EVTYPE)

# STORM
StormDataTidy$EVTYPE <- gsub('.*STORM.*', 'STORM', StormDataTidy$EVTYPE)

# SUMMARY
StormDataTidy$EVTYPE <- gsub('.*SUMMARY.*', 'SUMMARY', StormDataTidy$EVTYPE)

# TORNADO
StormDataTidy$EVTYPE <- gsub('.*TORNADO.*', 'TORNADO', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*TORNDAO.*', 'TORNADO', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*LANDSPOUT.*', 'TORNADO', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*WATERSPOUT.*', 'TORNADO', StormDataTidy$EVTYPE)

# SURF
StormDataTidy$EVTYPE <- gsub('.*SURF.*', 'SURF', StormDataTidy$EVTYPE)

# VOLCANIC
StormDataTidy$EVTYPE <- gsub('.*VOLCANIC.*', 'VOLCANIC', StormDataTidy$EVTYPE)

# WET
StormDataTidy$EVTYPE <- gsub('.*WET.*', 'WET', StormDataTidy$EVTYPE)

# WIND
StormDataTidy$EVTYPE <- gsub('.*WIND.*', 'WIND', StormDataTidy$EVTYPE)

# WINTER
StormDataTidy$EVTYPE <- gsub('.*WINTER.*', 'WINTER', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*WINTRY.*', 'WINTER', StormDataTidy$EVTYPE)
StormDataTidy$EVTYPE <- gsub('.*SNOW.*', 'WINTER', StormDataTidy$EVTYPE)

length(unique(StormDataTidy$EVTYPE))

#Tidy Up Economic Data
table(toupper(StormDataTidy$PROPDMGEXP))
table(toupper(StormDataTidy$CROPDMGEXP))

#Function to get Multiplier Factor

getMultiplier <- function(exp) {
  exp <- toupper(exp);
  if (exp == "")  return (10^0);
  if (exp == "-") return (10^0);
  if (exp == "?") return (10^0);
  if (exp == "+") return (10^0);
  if (exp == "0") return (10^0);
  if (exp == "1") return (10^1);
  if (exp == "2") return (10^2);
  if (exp == "3") return (10^3);
  if (exp == "4") return (10^4);
  if (exp == "5") return (10^5);
  if (exp == "6") return (10^6);
  if (exp == "7") return (10^7);
  if (exp == "8") return (10^8);
  if (exp == "9") return (10^9);
  if (exp == "H") return (10^2);
  if (exp == "K") return (10^3);
  if (exp == "M") return (10^6);
  if (exp == "B") return (10^9);
  return (NA);
}

# Calculate property damage and crop damage costs in billions
StormDataTidy$PROP_COST <- with(StormDataTidy, as.numeric(PROPDMG) * sapply(PROPDMGEXP, getMultiplier))/10^9
StormDataTidy$CROP_COST <- with(StormDataTidy, as.numeric(CROPDMG) * sapply(CROPDMGEXP, getMultiplier))/10^9
#Summarise Data

#Create a summarised dataset of health impact data (fatalities and injuries) and sort the results in descending order by health impact.

HealthImpact <- aggregate(x = list(HEALTH_IMPACT = StormDataTidy$FATALITIES + StormDataTidy$INJURIES),
                          by = list(EVENT_TYPE = StormDataTidy$EVTYPE),
                          FUN = sum,
                          na.rm = TRUE)
HealthImpact <- HealthImpact[order(HealthImpact$HEALTH_IMPACT, decreasing = TRUE),]
#Create a summarised dataset of damage impact costs and sort the results in descending order.

DamageCost <- aggregate(x = list(DMG_IMPACT = StormDataTidy$PROP_COST + StormDataTidy$CROP_COST),
                        by = list(EVENT_TYPE = StormDataTidy$EVTYPE),
                        FUN = sum,
                        na.rm = TRUE)
DamageCost <- DamageCost[order(DamageCost$DMG_IMPACT, decreasing = TRUE),]
#Results

Graph1 <- ggplot(HealthImpact[1:10,], aes(x=reorder(EVENT_TYPE, HEALTH_IMPACT),y=HEALTH_IMPACT,color=EVENT_TYPE)) + 
  coord_flip() +
  geom_bar(stat="identity", fill="white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event") + ylab("Number of fatalities and injuries") +
  theme(legend.position="none") +
  ggtitle("Top 10 Most Harmful Weather Events to Population Health")

Graph1

Graph2 <- ggplot(DamageCost[1:10,], aes(x=reorder(EVENT_TYPE, DMG_IMPACT),y=DMG_IMPACT,color=EVENT_TYPE)) + 
  coord_flip() +
  geom_bar(stat="identity", fill="white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event") + ylab("Property/Crop Damage in Billions") +
  theme(legend.position="none") +
  ggtitle("Top 10 Most Harmful Weather Events to US Economy")

Graph2