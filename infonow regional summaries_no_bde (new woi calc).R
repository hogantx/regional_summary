setwd("C:/Users/bhogan/Documents/R/Data")
library(ggplot2)
library(scales)
library(xts)
library(zoo)
library(tseries)
library(reshape)
library(forecast)
library(rattle)
library(manipulate)
library(googleVis)
library(ggthemes)
library(lattice)
library(dplyr)

# Two files are required:
#	regional_summary_desktop.csv
#	regional_summary_aib.csv

##	The code saves 16 files:
##
##				    AIB	Desktop	
##		Total			2	2		by region, by device
##		APAC			1	1	
##		EMEA			2	2		E/W
##		Greater China	1	1	
##		Latin America	1	1	
##		North America	1	1	
##


### Determine week_number
d <- as.numeric(Sys.Date())
w <- ceiling((d - 16431)/7)
week_number <- ifelse(w%%13 == 0, 13, w%%13) - 1
week_number


### Set image dimension
dev.new(width=10, height=5)


#############################
####### Desktop #############
#############################

regional_summary_data <- read.csv("regional_summary_data_desktop.csv")
head(regional_summary_data)
regional_summary_m <- as.data.frame(melt(regional_summary_data, id=1:4))
head(regional_summary_m)
regional_summary_m <- na.omit(regional_summary_m)


### Add "q" and "measure"
regional_summary_m$Quarter <- substr(regional_summary_m$variable, 2, 5)
regional_summary_m$measure <- substr(regional_summary_m$variable, 7, 9)
regional_summary_m <- na.omit(regional_summary_m)
regional_summary_m <- regional_summary_m[!(regional_summary_m$Quarter == "4q15" &
  regional_summary_m$w. > week_number + 1), ]

head(regional_summary_m)


### Summary Regional Sell Out ###
summary <- cast(regional_summary_m, MRVP + Quarter + w. ~ measure, sum)
summary <- summary[!(summary$MRVP=="Data-Not-Available"), ]
pos <- subset(summary, select=-c(EoH))
eoh <- subset(summary, select=-c(POS))

head(summary)


woi <- summary %>%
  filter((Quarter == "4q15" & w. == week_number) |(Quarter == "3q15" & w. == 13))
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(MRVP) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"


pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]
head(woi)



ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ MRVP) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue")) +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") + mtext("10 of them")

savePlot("total_1_desktop", "wmf", device = dev.cur())

### Summary Regional Sell Out ###
summary <- cast(regional_summary_m, Device. + Quarter + w. ~ measure, sum)
summary <- summary %>% 
  filter(Device. != "Other" & 
    Device. != "Gv No GPU" & 
    Device. != "Tn No GPU" &  
    Device. != "Gv1")

pos <- subset(summary, select=-c(EoH))
eoh <- subset(summary, select=-c(POS))

woi <- summary %>%
  filter((Quarter == "4q15" & w. == week_number) |(Quarter == "3q15" & w. == 13))
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"
head(woi)
woi <- woi[complete.cases(woi), ]

pos <- na.omit(pos)
eoh <- na.omit(eoh)
pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
head(pos)


eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]
eoh <- eoh[!(eoh$EoH == 0), ]
head(eoh)

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue")) +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("total_2_desktop", "wmf", device = dev.cur())


### APAC ###
apac <- subset(regional_summary_m, MRVP=="APAC")
apac_c <- cast(apac, Device. + Quarter + w. ~ measure, sum)
apac_c <- apac_c[!
  (apac_c$Device.=="Other"|
  apac_c$Device.=="Bm/Ml"|
  apac_c$Device.=="Kv No GPU"|
  apac_c$Device.=="Tn No GPU"), ]

pos <- subset(apac_c, select=-c(EoH))
eoh <- subset(apac_c, select=-c(POS))

pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]
head(pos)

woi <- apac_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

woi <- woi[complete.cases(woi), ]



ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("apac_desktop", "wmf", device = dev.cur())


### EMEA ###
EMEA <- subset(regional_summary_m, MRVP=="EMEA")
EMEA_c <- cast(EMEA, Device. + Quarter + w. ~ measure, sum)
EMEA_c <- EMEA_c[!
  (EMEA_c$Device.=="Other")|(EMEA_c$Device.=="Tn No GPU")|(EMEA_c$Device.=="Bm/Ml"), ]

EMEA_c <- EMEA_c %>%
  filter(Device. != "Other" & Device. != "Tn No GPU" & Device. != "Bm/Ml" )

pos <- subset(EMEA_c, select=-c(EoH))
eoh <- subset(EMEA_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- EMEA_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

woi <- woi[complete.cases(woi), ]
ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU")

savePlot("emea_1_desktop", "wmf", device = dev.cur())



### EMEA (Eastern / Western)###
EMEA <- subset(regional_summary_m, MRVP=="EMEA")
head(EMEA)
str(EMEA)
EMEA_c <- cast(EMEA, MRVP. + Device. + Quarter + w. ~ measure, sum)

levels(EMEA_c$MRVP.)[levels(EMEA_c$MRVP.)=="E"] <- "Eastern Europe"
levels(EMEA_c$MRVP.)[levels(EMEA_c$MRVP.)=="W"] <- "Western Europe"

EMEA_c <- EMEA_c %>%
  filter(Device. != "Other" & Device. != "Tn No GPU")

pos <- subset(EMEA_c, select=-c(EoH))
eoh <- subset(EMEA_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)
pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- EMEA_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device., MRVP.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

woi <- woi[complete.cases(woi), ]

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid(MRVP. ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU")

savePlot("emea_2_desktop", "wmf", device = dev.cur())


### Greater China ###
greater_china <- subset(regional_summary_m, MRVP=="Greater China")
greater_china_c <- cast(greater_china, Device. + Quarter + w. ~ measure, sum)
greater_china_c <- greater_china_c[!
  (greater_china_c$Device.=="Bv/Kb/Tm"|
  greater_china_c$Device.=="Other"|
  greater_china_c$Device.=="Gv1"|
  greater_china_c$Device.=="Tn No GPU"), ]

pos <- subset(greater_china_c, select=-c(EoH))
eoh <- subset(greater_china_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- greater_china_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=6, angle=90))+ 
  theme(axis.text.y = element_text(size=10)) +
  theme(strip.text = element_text(size=10)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("greater_china_desktop", "wmf", device = dev.cur())



### Latin America ###
latin_america <- subset(regional_summary_m, MRVP=="Latin America")
latin_america_c <- cast(latin_america, Device. + Quarter + w. ~ measure, sum)
latin_america_c <- latin_america_c[!
  (latin_america_c$Device.=="Other"|
  latin_america_c$Device.=="Kv No GPU"|
  latin_america_c$Device.=="Tn No GPU"), ]

pos <- subset(latin_america_c, select=-c(EoH))
eoh <- subset(latin_america_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- latin_america_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))
woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"
woi <- woi[complete.cases(woi), ]

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("latin_america_desktop", "wmf", device = dev.cur())



### North America ###
north_america <- subset(regional_summary_m, MRVP=="North America")
north_america_c <- cast(north_america, Device. + Quarter + w. ~ measure, sum)
north_america_c <- north_america_c[!
  (north_america_c$Device.=="Other"|
  north_america_c$Device.=="Kv No GPU"|
  north_america_c$Device.=="Tn No GPU"), ]

pos <- subset(north_america_c, select=-c(EoH))
eoh <- subset(north_america_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- north_america_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))
woi$WoI <- round((13 + week_number) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"
woi <- woi[complete.cases(woi), ]



ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("north_america_desktop", "wmf", device = dev.cur())



#############################
####### AIB #############
#############################

regional_summary_data <- read.csv("regional_summary_data_aib.csv")
head(regional_summary_data)
regional_summary_m <- as.data.frame(melt(regional_summary_data, id=1:4))
head(regional_summary_m)
regional_summary_m <- na.omit(regional_summary_m)


### Add "q" and "measure"
regional_summary_m$Quarter <- substr(regional_summary_m$variable, 2, 5)
regional_summary_m$measure <- substr(regional_summary_m$variable, 7, 9)
regional_summary_m <- na.omit(regional_summary_m)
regional_summary_m <- regional_summary_m[!(regional_summary_m$Quarter == "4q15" &
  regional_summary_m$w. > week_number + 1), ]

head(regional_summary_m)


### Summary Regional Sell Out ###
summary <- cast(regional_summary_m, MRVP + Quarter + w. ~ measure, sum)
summary <- summary[!(summary$MRVP=="Data-Not-Available"), ]
pos <- subset(summary, select=-c(EoH))
eoh <- subset(summary, select=-c(POS))

head(summary)


woi <- summary %>%
  filter((Quarter == "4q15" & w. == week_number) |(Quarter == "3q15" & w. == 13))
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(MRVP) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"


pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number + 1), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]
head(woi)


ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ MRVP) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue")) +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("total_1_aib", "wmf", device = dev.cur())

### Summary Regional Sell Out ###
summary <- cast(regional_summary_m, Device. + Quarter + w. ~ measure, sum)
summary <- summary %>% 
  filter(Device. != "Other" & 
    Device. != "Gv No GPU" & 
    Device. != "Tn No GPU" &  
    Device. != "Gv1")

pos <- subset(summary, select=-c(EoH))
eoh <- subset(summary, select=-c(POS))

woi <- summary %>%
  filter((Quarter == "4q15" & w. == week_number) |(Quarter == "3q15" & w. == 13))
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"
head(woi)
woi <- woi[complete.cases(woi), ]

pos <- na.omit(pos)
eoh <- na.omit(eoh)
pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number + 1), ]
head(pos)


eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]
eoh <- eoh[!(eoh$EoH == 0), ]
head(eoh)

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue")) +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("total_2_aib", "wmf", device = dev.cur())


### APAC ###
apac <- subset(regional_summary_m, MRVP=="APAC")
apac_c <- cast(apac, Device. + Quarter + w. ~ measure, sum)
apac_c <- apac_c[!
  (apac_c$Device.=="Other"|
  apac_c$Device.=="Bm/Ml"|
  apac_c$Device.=="Kv No GPU"|
  apac_c$Device.=="Tn No GPU"), ]

pos <- subset(apac_c, select=-c(EoH))
eoh <- subset(apac_c, select=-c(POS))

pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number + 1), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]
head(pos)

woi <- apac_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

woi <- woi[complete.cases(woi), ]



ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("apac_aib", "wmf", device = dev.cur())


### EMEA ###
EMEA <- subset(regional_summary_m, MRVP=="EMEA")
EMEA_c <- cast(EMEA, Device. + Quarter + w. ~ measure, sum)
EMEA_c <- EMEA_c[!
  (EMEA_c$Device.=="Other")|(EMEA_c$Device.=="Tn No GPU")|(EMEA_c$Device.=="Bm/Ml"), ]

EMEA_c <- EMEA_c %>%
  filter(Device. != "Other" & Device. != "Tn No GPU" & Device. != "Bm/Ml" )

pos <- subset(EMEA_c, select=-c(EoH))
eoh <- subset(EMEA_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number+1), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- EMEA_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

woi <- woi[complete.cases(woi), ]
ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU")

savePlot("emea_1_aib", "wmf", device = dev.cur())


### EMEA (Eastern / Western)###
EMEA <- subset(regional_summary_m, MRVP=="EMEA")
head(EMEA)
str(EMEA)
EMEA_c <- cast(EMEA, MRVP. + Device. + Quarter + w. ~ measure, sum)

levels(EMEA_c$MRVP.)[levels(EMEA_c$MRVP.)=="E"] <- "Eastern Europe"
levels(EMEA_c$MRVP.)[levels(EMEA_c$MRVP.)=="W"] <- "Western Europe"

EMEA_c <- EMEA_c %>%
  filter(Device. != "Other" & Device. != "Tn No GPU")

pos <- subset(EMEA_c, select=-c(EoH))
eoh <- subset(EMEA_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)
pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number+1), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- EMEA_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device., MRVP.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

woi <- woi[complete.cases(woi), ]

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid(MRVP. ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU")

savePlot("emea_2_aib", "wmf", device = dev.cur())


### Greater China ###
greater_china <- subset(regional_summary_m, MRVP=="Greater China")
greater_china_c <- cast(greater_china, Device. + Quarter + w. ~ measure, sum)
greater_china_c <- greater_china_c[!
  (greater_china_c$Device.=="Bv/Kb/Tm"|
  greater_china_c$Device.=="Other"|
  greater_china_c$Device.=="Gv1"|
  greater_china_c$Device.=="Tn No GPU"), ]

pos <- subset(greater_china_c, select=-c(EoH))
eoh <- subset(greater_china_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number+1), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- greater_china_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))

woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=6, angle=90))+ 
  theme(axis.text.y = element_text(size=10)) +
  theme(strip.text = element_text(size=10)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("greater_china_aib", "wmf", device = dev.cur())


### Latin America ###
latin_america <- subset(regional_summary_m, MRVP=="Latin America")
latin_america_c <- cast(latin_america, Device. + Quarter + w. ~ measure, sum)
latin_america_c <- latin_america_c[!
  (latin_america_c$Device.=="Other"|
  latin_america_c$Device.=="Kv No GPU"|
  latin_america_c$Device.=="Tn No GPU"), ]

pos <- subset(latin_america_c, select=-c(EoH))
eoh <- subset(latin_america_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number +1 ), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- latin_america_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))
woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"
woi <- woi[complete.cases(woi), ]

ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("latin_america_aib", "wmf", device = dev.cur())



### North America ###
north_america <- subset(regional_summary_m, MRVP=="North America")
north_america_c <- cast(north_america, Device. + Quarter + w. ~ measure, sum)
north_america_c <- north_america_c[!
  (north_america_c$Device.=="Other"|
  north_america_c$Device.=="Kv No GPU"|
  north_america_c$Device.=="Tn No GPU"), ]

pos <- subset(north_america_c, select=-c(EoH))
eoh <- subset(north_america_c, select=-c(POS))
pos <- na.omit(pos)
eoh <- na.omit(eoh)

pos <- pos[!(pos$Quarter=="4q15" & pos$w. > week_number + 1), ]
eoh <- eoh[!(eoh$Quarter=="4q15" & eoh$w. > week_number), ]

head(pos)

woi <- north_america_c %>%
  filter((Quarter == "4q15" & w. == week_number)| w. == 13)
head(woi)
woi$EoH <- ifelse(woi$Quarter == "3q15", 0, woi$EoH)
head(woi)

woi <- woi %>%
  group_by(Device.) %>%
  summarize(EoH = sum(EoH), POS = sum(POS))
woi$WoI <- round((13 + week_number + 1) * woi$EoH / woi$POS, 0)
woi$w. <- week_number
woi$Quarter <- "4q15"
woi <- woi[complete.cases(woi), ]



ggplot(eoh, aes(factor(w.), EoH, fill=Quarter)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("gray75", "skyblue")) +
  facet_grid( ~ Device.) +
  geom_text(data=woi, aes(factor(w.), EoH, label=WoI), hjust=0.5, size=2.5, colour="blue") +
  geom_line(data=pos, aes(factor(w.), POS, group=Quarter, colour=Quarter)) +
  scale_colour_manual(values=c("gray40", "blue"))  +
  geom_point(data=pos, aes(factor(w.), POS, shape=Quarter), size=1.5) +
  scale_shape_manual(values=c(NA, 20)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(size=9, angle=90))+ 
  theme(axis.text.y = element_text(size=9)) +
  theme(strip.text = element_text(size=9)) +
  xlab("Week") + ylab("Infonow kU") 

savePlot("north_america_aib", "wmf", device = dev.cur())




