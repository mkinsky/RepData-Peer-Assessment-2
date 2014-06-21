rm(list=ls())   #Clear all objects

require(dplyr)
require(stringr)

#Set the working directory and file vectors
working.dir <- "C://Education//Reproducible Research//Homework//Peer Assessment 2"
setwd(working.dir)

input.dir <- ".\\input\\"
output.dir <- ".\\output\\"
script.dir <- ".\\scripts\\"
input.storm.data <- "repdata-data-StormData.csv"
input.storm.ref.data <- "storm-reference-data.csv"

#Load the data
storm.data <- read.csv(paste(input.dir, input.storm.data, sep="\\"), stringsAsFactors = FALSE, strip.white = TRUE)
storm.ref.data <- read.csv(paste(input.dir, input.storm.ref.data, sep="\\"), stringsAsFactors = FALSE, strip.white = TRUE)
#Profile the data
#summary(storm.data)
#str(storm.data)
#View(storm.data)

#subeset storm.data where pop.health > 0 | PROPDMG > 0 | CROPDMG > 0
storm.data.step.1 <- storm.data %.%
  mutate(pop.health = FATALITIES + INJURIES, 
         prop.dmg.exp = 0,
         prop.dmg.value = 0,
         crop.dmg.exp = 0,
         crop.dmg.value = 0,
         total.dmg.value = 0,
         event.type.id = as.integer(0)) %.%
  select(event.type.id, EVTYPE, FATALITIES, INJURIES, pop.health, PROPDMG, PROPDMGEXP, prop.dmg.exp, prop.dmg.value, CROPDMG, CROPDMGEXP, crop.dmg.exp, crop.dmg.value, total.dmg.value) %.%
  filter(pop.health > 0 | PROPDMG > 0 | CROPDMG > 0)

#rm(storm.data)

storm.data.step.1$PROPDMGEXP = toupper(storm.data.step.1$PROPDMGEXP)
storm.data.step.1$CROPDMGEXP = toupper(storm.data.step.1$CROPDMGEXP)
storm.data.step.1$EVTYPE <- str_trim(storm.data.step.1$EVTYPE)

storm.data.step.1[storm.data.step.1$PROPDMGEXP == "1", c("prop.dmg.exp")] = 10
storm.data.step.1[storm.data.step.1$PROPDMGEXP %in% c("H", "2"), c("prop.dmg.exp")] = 100
storm.data.step.1[storm.data.step.1$PROPDMGEXP %in% c("K", "3"), c("prop.dmg.exp")] = 1000
storm.data.step.1[storm.data.step.1$PROPDMGEXP == "4", c("prop.dmg.exp")] = 10000
storm.data.step.1[storm.data.step.1$PROPDMGEXP == "5", c("prop.dmg.exp")] = 100000
storm.data.step.1[storm.data.step.1$PROPDMGEXP %in% c("M", "6"), c("prop.dmg.exp")] = 1000000
storm.data.step.1[storm.data.step.1$PROPDMGEXP == "7", c("prop.dmg.exp")] = 10000000
storm.data.step.1[storm.data.step.1$PROPDMGEXP == "8", c("prop.dmg.exp")] = 100000000
storm.data.step.1[storm.data.step.1$PROPDMGEXP %in% c("B", "9"), c("prop.dmg.exp")] = 1000000000

storm.data.step.1[storm.data.step.1$CROPDMGEXP == "1", c("crop.dmg.exp")] = 10
storm.data.step.1[storm.data.step.1$CROPDMGEXP %in% c("H", "2"), c("crop.dmg.exp")] = 100
storm.data.step.1[storm.data.step.1$CROPDMGEXP %in% c("K", "3"), c("crop.dmg.exp")] = 1000
storm.data.step.1[storm.data.step.1$CROPDMGEXP == "4", c("crop.dmg.exp")] = 10000
storm.data.step.1[storm.data.step.1$CROPDMGEXP == "5", c("crop.dmg.exp")] = 100000
storm.data.step.1[storm.data.step.1$CROPDMGEXP %in% c("M", "6"), c("crop.dmg.exp")] = 1000000
storm.data.step.1[storm.data.step.1$CROPDMGEXP == "7", c("crop.dmg.exp")] = 10000000
storm.data.step.1[storm.data.step.1$CROPDMGEXP == "8", c("crop.dmg.exp")] = 100000000
storm.data.step.1[storm.data.step.1$CROPDMGEXP %in% c("B", "9"), c("crop.dmg.exp")] = 1000000000

storm.data.step.1$prop.dmg.value = as.numeric(storm.data.step.1$PROPDMG) * as.numeric(storm.data.step.1$prop.dmg.exp)
storm.data.step.1$crop.dmg.value = as.numeric(storm.data.step.1$CROPDMG) * as.numeric(storm.data.step.1$crop.dmg.exp)
storm.data.step.1$total.dmg.value = storm.data.step.1$prop.dmg.value + storm.data.step.1$crop.dmg.value

#write.csv(x = storm.data.step.1, file = paste(working.dir, output.dir, "stormDataStep1.csv", sep = "//"), row.names = F)

storm.data.step.2 <- storm.data.step.1 %.%  
  group_by(EVTYPE) %.%
  summarise(event.type.id = sum(event.type.id), sum.pop.health = sum(pop.health), sum.prop.dmg.value = sum(prop.dmg.value), sum.crop.dmg.value = sum(crop.dmg.value), sum.total.dmg.value = sum(total.dmg.value)) %.%
  select(event.type.id, EVTYPE, sum.pop.health, sum.prop.dmg.value, sum.crop.dmg.value, sum.total.dmg.value)

#rm(storm.data.step.1)

storm.data.step.2$id = as.numeric(rownames(storm.data.step.2))
storm.data.step.2$event.type.id = as.integer(storm.data.step.2$event.type.id)

#Set category values
storm.data.step.2[grep("LIGHT", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 29
storm.data.step.2[grep("LIGNTNING", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 29
storm.data.step.2[grep("SLIDE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 6
storm.data.step.2[grep("Landslump", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 6
storm.data.step.2[grep("WIND", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 38
storm.data.step.2[grep("WATER", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 45
storm.data.step.2[grep("TORNADO", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 40
storm.data.step.2[grep("TORNDAO", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 40
storm.data.step.2[grep("LANDSPOUT", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 40
storm.data.step.2[grep("GUSTNADO", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 40
storm.data.step.2[grep("TSUNAMI", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 43
storm.data.step.2[grep("TROPICAL STORM", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 42
storm.data.step.2[grep("TROPICAL DEPRESSION", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 41
storm.data.step.2[grep("THUNDER", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 39
storm.data.step.2[grep("BLIZZARD", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 3
storm.data.step.2[grep("FREEZE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 17
storm.data.step.2[grep("FREEZI", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 17
storm.data.step.2[grep("FROST", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 17
storm.data.step.2[grep("HYPERTHERMIA/EXPOSURE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 17
storm.data.step.2[grep("HYPOTHERMIA", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 17
storm.data.step.2[grep("LOW TEMPERATURE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 17
storm.data.step.2[grep("ASTRONOMICAL", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 1
storm.data.step.2[grep("AVALANC", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 2
storm.data.step.2[grep("HURRIC", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 25
storm.data.step.2[grep("TYPHOON", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 25
storm.data.step.2[grep("ICE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 26
storm.data.step.2[grep("ICY ROADS", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 26
storm.data.step.2[grep("GLAZE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 25
storm.data.step.2[grep("RAIN", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 21
storm.data.step.2[grep("HEAVY MIX", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 21
storm.data.step.2[grep("HEAVY SEA", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 21
storm.data.step.2[grep("HEAVY SHOWER", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 21
storm.data.step.2[grep("HEAVY PRECIPITATION", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 21

storm.data.step.2[grep("HEAVY SURF", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("HIGH SURF", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("ROUGH SURF", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("ROUGH SEA", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("HIGH SWELLS", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("HEAVY SWELLS", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("HIGH TIDE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("HIGH WAVES", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23
storm.data.step.2[grep("HIGH SEA", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 23

storm.data.step.2[grep("SLEET", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 36
storm.data.step.2[grep("SNOW", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 22
storm.data.step.2[grep("FLOOD", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 15
storm.data.step.2[grep("URBAN", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 15
storm.data.step.2[grep("WETNESS", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 15
storm.data.step.2[grep("FLASH FLOOD", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 14
storm.data.step.2[grep("COASTAL FLOOD", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 4
storm.data.step.2[grep("LAKE FLOOD", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 28
storm.data.step.2[grep("LAKESHORE FLOOD", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 28
storm.data.step.2[grep("FOG", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 7
storm.data.step.2[grep("DENSE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 7
storm.data.step.2[grep("FREEZING FOG", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 16
storm.data.step.2[grep("DUST", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 10
storm.data.step.2[grep("DUST STORM", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 11
storm.data.step.2[grep("WINTER STORM", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 47
storm.data.step.2[grep("WINTER WEATHER", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 48
storm.data.step.2[grep("WINTRY MIX", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 48
storm.data.step.2[grep("MIXED PRECIP", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 48
storm.data.step.2[grep("FIRE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 46
storm.data.step.2[grep("COLD", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 5
storm.data.step.2[grep("COOL", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 5
storm.data.step.2[grep("WIND CHILL", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 5
storm.data.step.2[grep("WINDCHILL", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 5
storm.data.step.2[grep("HEAT", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 20
storm.data.step.2[grep("UNSEASONABLY WARM", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 20
storm.data.step.2[grep("WARM WEATHER", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 20
storm.data.step.2[grep("DROUGHT", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 9
storm.data.step.2[grep("Beach Erosion", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("COASTAL EROSION", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("Erosion/Cstl Flood", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("COASTAL STORM", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("COASTALSTORM", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("COASTAL SURGE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("HAZARDOUS SURF", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("ROGUE WAVE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37
storm.data.step.2[grep("STORM SURGE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 37

storm.data.step.2[grep("DAM BREAK", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 14
storm.data.step.2[grep("DOWNBURST", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 14
storm.data.step.2[grep("HAIL", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 19
storm.data.step.2[grep("MARINE HAIL", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 30
storm.data.step.2[grep("RIP CURRENT", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 34
storm.data.step.2[grep("DROWNING", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 34
storm.data.step.2[grep("BURST", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 39
storm.data.step.2[grep("FUNNEL", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 18
storm.data.step.2[grep("SEICHE", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 35

storm.data.step.2[grep("HIGH WIND", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 24
storm.data.step.2[grep("STRONG WIND", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 38
storm.data.step.2[grep("MARINE HIGH WIND", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 31
storm.data.step.2[grep("MARINE STRONG WIND", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 32
storm.data.step.2[grep("MARINE THUNDERSTORM WIND", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 33
storm.data.step.2[grep("THUNDERSTORM WIND", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 39
storm.data.step.2[grep("TSTM", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 39
storm.data.step.2[grep("VOLCANIC ASH", storm.data.step.2$EVTYPE, ignore.case=T), c("event.type.id")] <- 44

storm.ref.data$event.type.id = as.integer(storm.ref.data$event.type.id)
storm.data.step.2$event.type.id = as.integer(storm.data.step.2$event.type.id)
storm.data.step.2 <- left_join(storm.data.step.2, storm.ref.data)

storm.data.step.3 <- storm.data.step.2 %.%  
  group_by(event.type.group, event.name) %.%
  summarise(sum.pop.health = sum(sum.pop.health), sum.prop.dmg.value = sum(sum.prop.dmg.value), sum.crop.dmg.value = sum(sum.crop.dmg.value), sum.total.dmg.value = sum(sum.total.dmg.value)) %.%
  select(event.type.group, event.name, sum.pop.health, sum.prop.dmg.value, sum.crop.dmg.value, sum.total.dmg.value)

#storm.data.step.4 <- storm.data.step.3 %.%  
#  group_by(event.type.group) %.%
#  summarise(sum.pop.health = sum(sum.pop.health), sum.prop.dmg.value = sum(sum.prop.dmg.value), sum.crop.dmg.value = sum(sum.crop.dmg.value), sum.total.dmg.value = sum(sum.total.dmg.value), event.count = n()) %.%
#  select(event.type.group, event.count, sum.pop.health, sum.prop.dmg.value, sum.crop.dmg.value, sum.total.dmg.value)

sum.pop.health <- sum(storm.data.step.3$sum.pop.health) 
max.pop.health <- max(storm.data.step.3$sum.pop.health)
diff.pop.health <- sum.pop.health - max.pop.health

max.pop.health.event <- as.character(subset(storm.data.step.3, sum.pop.health == max.pop.health, select = c(event.name)))
other.pop.health.event <- "All other event types combined"
names.argument <- as.vector(c(max.pop.health.event, other.pop.health.event))
bar.plot.data <- as.data.frame(rbind(max.pop.health, diff.pop.health))
rownames(bar.plot.data) <- NULL
colnames(bar.plot.data) <- c("pop.health")
bar.plot.data$pop.health <- as.numeric(bar.plot.data$pop.health)
x.label <- "Weather Event"
y.label <- "Population Health (# of Injuries and Fatalities)"
y.limit <- c(0, 100000)
main.title <- "Total Number of U.S. Weather Realted Injuries/Deaths (1950 - 2011)"
barplot(bar.plot.data$pop.health, names.arg = names.argument, xlab = x.label, ylab = y.label, ylim = y.limit, main = main.title)

max(storm.data.step.3$sum.pop.health)/diff.pop.health #more than 1.6x larger

df <- storm.data.step.3[ order(-storm.data.step.3[,6]), c(2,6)]
barplot(df[1:10, c(2)], names.arg = df[1:10, c(1)])

storm.data.step.3 %.%
  select(event.type.group, event.name, sum.pop.health, sum.total.dmg.value) %.%
  arrange(desc(sum.pop.health))

storm.data.step.4 <- storm.data.step.3[, c("event.name", "sum.pop.health", "sum.total.dmg.value")]
colnames(storm.data.step.4) <- c("Event Name", "Number of Injuries and Fatalities", "Property and Crop Damage ($)")

storm.data.step.4[ order(-storm.data.step.4[,3]), ]

#write.csv(x = storm.data.step.4, file = paste(working.dir, output.dir, "stormDataV4.csv", sep = "//"), row.names = F)