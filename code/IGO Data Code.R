#CHINA IGO DATA

rm(list=ls(all=TRUE))
setwd("/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project")

require(ggplot2)
require(tidyverse)
require(rvest)
require(readxl)
require(dplyr)
require(fuzzyjoin)
require(countrycode)
library(RColorBrewer)
require(readr)

#GOVERNMENT DATA
Intl_Gov_Data <- read_excel("Government Data.xls")
#IGO DATA
IGO_Data <- read.csv("dyadic_formatv3.csv") #trust me you dont want to load this

#CLEANING DATA
country_code <- data.frame(Intl_Gov_Data$ccode, Intl_Gov_Data$country, Intl_Gov_Data$un_continent,
                           Intl_Gov_Data$un_region, Intl_Gov_Data$regime_r, Intl_Gov_Data$year) #new df with necessary info
country_code <- distinct(country_code) #clean
country_code <- country_code %>% rename(ccode2 = Intl_Gov_Data.ccode, 
                                        country = Intl_Gov_Data.country,
                                        un_continent = Intl_Gov_Data.un_continent,
                                        un_region = Intl_Gov_Data.un_region,
                                        regime = Intl_Gov_Data.regime_r,
                                        year = Intl_Gov_Data.year) #rename columns
country_code <- subset(country_code, year >= "2000") # 2000-2012 timeline

IGO_Data <- subset(IGO_Data, year>=2000) #set after year 2000
China_IGO1 <- subset(IGO_Data, country1 == "CHN") #subset group 1
China_IGO2 <- subset(IGO_Data, country2 == "CHN") #subset group 2
China_IGO2 <- rename(China_IGO2, country1 = country2, country2 = country1, ccode1 = ccode2, ccode2 = ccode1) #rename variables to match across data sets
China_IGO <- rbind(China_IGO1, China_IGO2) #join data sets
China_IGO <- merge(China_IGO, country_code, by = c("ccode2", "year")) #merge with government info

China_IGO_Dummy <- China_IGO[-c(1:6, 540:544)] #create dummy with only participation
China_IGO_Dummy <- replace(China_IGO_Dummy, China_IGO_Dummy<0,0) #set so incompletes/unknown are 0
allignment <- rowSums(China_IGO_Dummy) #add across all rows
China_IGO <- data.frame(China_IGO, allignment) #combine value into dataframe
China_IGO$allignment <- China_IGO$allignment/100 #standardize out of 0-1

China_WAfrica_IGO <- subset(China_IGO, un_region == "Western Africa") #only West Africa
China_NAfrica_IGO <- subset(China_IGO, un_region == "Northern Africa") #only North Africa
China_MAfrica_IGO <- subset(China_IGO, un_region == "Middle Africa") #only Middle Africa
China_EAfrica_IGO <- subset(China_IGO, un_region == "Eastern Africa") #only East Africa
China_SAfrica_IGO <- subset(China_IGO, un_region == "Southern Africa") #only South Africa
China_SAmerica_IGO <- subset(China_IGO, un_region == "South America") #only South America

#exports
write_csv(China_IGO, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China IGO.csv")
write_csv(China_SAmerica_IGO, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China South America IGO.csv")
write_csv(China_WAfrica_IGO, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China WAfrica IGO.csv")
write_csv(China_NAfrica_IGO, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China NAfrica IGO.csv")
write_csv(China_MAfrica_IGO, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China MAfrica IGO.csv")
write_csv(China_EAfrica_IGO, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China EAfrica IGO.csv")
write_csv(China_SAfrica_IGO, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China SAfrica IGO.csv")



#GRAPHS
#making large color palette
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

#West Africa and China IGO Alignment
WAfrica_IGO_Graph <- ggplot(China_WAfrica_IGO, aes(x = year, y = allignment, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  ylim(.1,.75) +
  labs(title = "IGO Alignment in Western Africa", x = "Year 2000-2012", y = "Degree of Alignment from 0-1")
WAfrica_IGO_Graph

#Middle Africa and China IGO Alignment
MAfrica_IGO_Graph <- ggplot(China_MAfrica_IGO, aes(x = year, y = allignment, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  ylim(.1,.75) +
  labs(title = "IGO Alignment in Middle Africa", x = "Year 2000-2012", y = "Degree of Alignment from 0-1")
MAfrica_IGO_Graph

#East Africa and China IGO Alignment
EAfrica_IGO_Graph <- ggplot(China_EAfrica_IGO, aes(x = year, y = allignment, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  ylim(.1,.75) +
  labs(title = "IGO Alignment in Eastern Africa", x = "Year 2000-2012", y = "Degree of Alignment from 0-1")
EAfrica_IGO_Graph

#North Africa and China IGO Alignment
NAfrica_IGO_Graph <- ggplot(China_NAfrica_IGO, aes(x = year, y = allignment, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  ylim(.1,.75) +
  labs(title = "IGO Alignment in Northern Africa", x = "Year 2000-2012", y = "Degree of Alignment from 0-1")
NAfrica_IGO_Graph

#Southern Africa and China IGO Alignment
SAfrica_IGO_Graph <- ggplot(China_SAfrica_IGO, aes(x = year, y = allignment, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  ylim(.1,.75) +
  labs(title = "IGO Alignment in Southern Africa", x = "Year 2000-2012", y = "Degree of Alignment from 0-1")
SAfrica_IGO_Graph

#South America and China IGO Alignment
SAmerica_IGO_Graph <- ggplot(China_SAmerica_IGO, aes(x = year, y = allignment, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  ylim(.1,.75) +
  labs(title = "IGO Allignment in South America", x = "Year 2000-2012", y = "Degree of Allignment from 0-1")
SAmerica_IGO_Graph

Africa_IGO_Graph <- ggplot(China_Africa_IGO, aes(x = year, y = allignment, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  ylim(.1,.75) +
  labs(title = "IGO Allignment in Africa", x = "Year 2000-2012", y = "Degree of Alignment from 0-1")
Africa_IGO_Graph

ggsave("EAfrica China IGO.png", plot = EAfrica_IGO_Graph)
ggsave("MAfrica China IGO.png", plot = MAfrica_IGO_Graph)
ggsave("NAfrica China IGO.png", plot = NAfrica_IGO_Graph)
ggsave("SAfrica China IGO.png", plot = SAfrica_IGO_Graph)
ggsave("WAfrica China IGO.png", plot = WAfrica_IGO_Graph)
ggsave("SAmerica China IGO.png", plot = SAmerica_IGO_Graph)
ggsave("Africa China IGO.png", plot = Africa_IGO_Graph)


#ANALYSIS
setwd("/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/Cleaned Data")
China_IGO <- read.csv("China IGO.csv")
country_code <- read.csv("Country Codes.csv")

China_Africa_IGO <- subset(China_IGO, un_continent == "Africa")
China_WAfrica_IGO <- subset(China_IGO, un_region == "Western Africa") #only West Africa
China_NAfrica_IGO <- subset(China_IGO, un_region == "Northern Africa") #only North Africa
China_MAfrica_IGO <- subset(China_IGO, un_region == "Middle Africa") #only Middle Africa
China_EAfrica_IGO <- subset(China_IGO, un_region == "Eastern Africa") #only East Africa
China_SAfrica_IGO <- subset(China_IGO, un_region == "Southern Africa") #only South Africa
China_SAmerica_IGO <- subset(China_IGO, un_region == "South America") #only South America

#calculate averages across all years for each country in South America in new dataframe
Avg_China_SAmerica_IGO <- China_SAmerica_IGO %>%
  group_by(country) %>%
  summarise_at(vars(allignment), list(name = mean))

#calculate averages across all years for each country in Africa in new dataframe
Avg_China_Africa_IGO <- China_Africa_IGO %>%
  group_by(country) %>%
  summarise_at(vars(allignment), list(name = mean))

