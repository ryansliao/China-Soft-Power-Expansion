##China 
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

#UN DATA
UN_Votes <- read.csv("UNVotes-1.csv")
#GOVERNMENT DATA
Intl_Gov_Data <- read_excel("Government Data.xls")
#UN AGREE
UN_Agree <- read.csv("AgreementScoresAll_Jun2022.csv")


#cleaning data
UN_Votes <- UN_Votes[-c(2, 16, 17, 20:25)]
UN_Votes <- subset(UN_Votes, year>=2000)

UN_Agree <- subset(UN_Agree, year>=2000)

China_Votes <- subset(UN_Votes, Country == "CHN") 
China_Votes <- subset(China_Votes, session >= 26) #starting UN session 26
China_Votes <- China_Votes[-c(1)]

China_Agree <- subset(UN_Agree, ccode1 == "710") #beginning with UN session 26(1971)
China_Agree <- China_Agree[-c(1)] #remove column
China_Agree$ccode2 <- as.character(China_Agree$ccode2) #recode to character
China_Agree <- subset(China_Agree, year>=2000) #set years after 2000
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
China_Agree <- merge(China_Agree, country_code, by = c("ccode2", "year")) #join

China_Africa <- subset(China_Agree, un_continent == "Africa") #only Africa-China
China_WAfrica <- subset(China_Africa, un_region == "Western Africa") #Western Africa only
China_SAfrica <- subset(China_Africa, un_region == "Southern Africa") #Southern Africa only
China_MAfrica <- subset(China_Africa, un_region == "Middle Africa") #Middle Africa only
China_EAfrica <- subset(China_Africa, un_region == "Eastern Africa") #Eastern Africa only
China_NAfrica <- subset(China_Africa, un_region == "Northern Africa") #Northern Africa only

China_SAmerica <- subset(China_Agree, un_region == "South America") #only South America-China

#exports
write_csv(China_Agree, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China Agree.csv")
write_csv(country_code, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/Country Codes.csv")
write_csv(UN_Votes, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/UN Votes.csv")
write_csv(China_SAmerica, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China South America.csv")
write_csv(China_Africa, "/Users/callitullis/Documents/Fall 2022/POLI 178/Final Project/China Africa.csv")


#PLOTS

#large color palette creation
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

#Continental Africa Graph
Africa_China_UN <- ggplot(data = China_Africa, aes(x = year, y = agree, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  labs(title = "UN Voting Agreement Levels Across Africa", x = "Year 2000-2012", y = "Agreement")
Africa_China_UN

#Western Africa Regional Graph
WAfrica_China_UN <- ggplot(data = China_WAfrica, aes(x = year, y = agree, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2))+
  labs(title = "UN Voting Agreement Levels in Western Africa", x = "Year 2000-2012", y = "Agreement")
WAfrica_China_UN

#Southern Africa Regional Graph
SAfrica_China_UN <- ggplot(data = China_SAfrica, aes(x = year, y = agree, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  labs(title = "UN Voting Agreement Levels in Southern Africa", x = "Year 2000-2012", y = "Agreement")
SAfrica_China_UN

#Middle Africa Regional Graph
MAfrica_China_UN <- ggplot(data = China_MAfrica, aes(x = year, y = agree, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  labs(title = "UN Voting Agreement Levels in Middle Africa", x = "Year 2000-2012", y = "Agreement")
MAfrica_China_UN

#Eastern Africa Regional Graph
EAfrica_China_UN <- ggplot(data = China_EAfrica, aes(x = year, y = agree, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  labs(title = "UN Voting Agreement Levels in Eastern Africa", x = "Year 2000-2012", y = "Agreement")
EAfrica_China_UN

#Northern Africa Regional Graph
NAfrica_China_UN <- ggplot(data = China_NAfrica, aes(x = year, y = agree, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector) +
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  labs(title = "UN Voting Agreement Levels in Northern Africa", x = "Year 2000-2012", y = "Agreement")
NAfrica_China_UN

#South America Graph
SAmerica_China_UN <- ggplot(data = China_SAmerica, aes(x = year, y = agree, color = country)) +
  geom_line() +
  scale_color_manual(values = col_vector)+
  scale_x_continuous(breaks=seq(2000, 2012, 2)) +
  labs(title = "UN Voting Agreement Levels Across South America", x = "Year 2000-2012", y = "Agreement")
SAmerica_China_UN

ggsave("EAfrica China UN.png", plot = EAfrica_China_UN)
ggsave("MAfrica China UN.png", plot = MAfrica_China_UN)
ggsave("NAfrica China UN.png", plot = NAfrica_China_UN)
ggsave("SAfrica China UN.png", plot = SAfrica_China_UN)
ggsave("WAfrica China UN.png", plot = WAfrica_China_UN)
ggsave("SAmerica China UN.png", plot = SAmerica_China_UN)

#ANALYSIS
#averages across all years for each country in new dataframe
Avg_China_SAmerica <- China_SAmerica %>%
  group_by(country) %>%
  summarise_at(vars(agree), list(name = mean))

#averages across all years for each country in new dataframe
Avg_China_Africa <- China_Africa %>%
  group_by(country) %>%
  summarise_at(vars(agree), list(name = mean))


