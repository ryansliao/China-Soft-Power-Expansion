require(tidyverse)
require(lubridate)
require(ggplot2)
install.packages("ggplot2")


dat <- read_csv("C:/Users/Yan/Downloads/China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx - Dataset 1+2.csv")


colnames(dat) 
dat <- select(dat, -c("Investor", "Share Size", "Transaction Party", "Subsector", "Region", Greenfield)) 
dat$Year <- as.character(dat$Year)
dat$Year <- paste(dat$Month, dat$Year)
dat$Year <- lubridate::my(dat$Year)
dat <- select(dat, -c("Month"))
dat[]<-lapply(dat,gsub,pattern="$",fixed=TRUE,replacement="")
dat[]<-lapply(dat,gsub,pattern=",",fixed=TRUE,replacement="")
dat$`Quantity in Millions` <- as.numeric(dat$`Quantity in Millions`)
dat$BRI <- as.numeric(dat$BRI)
dat <- dat %>% replace(is.na(.), 0)
dat <- dat %>% map_df(rev)
write.csv(dat,"C:/Users/pc/Downloads/FDIdata.csv", row.names = FALSE)

### To modify the data shown by the map, trim tibble dat


country_code = data.frame(matrix(ncol = 2, nrow = length(unique(c(dat$Country)))))
names(country_code) = c("region","money_invested")  
class(country_code$region) = "Character"
country_code$money_invested <- as.numeric(country_code$money_invested)
country_code$region = unique(c(dat$Country))
country_code <- country_code %>% replace(is.na(.), 0)

country_code


for(x in country_code$region){
  dat_temp<- dat %>% filter(dat$Country == x)
  temp_sum <-sum(dat_temp[, "Quantity in Millions"])
  row_index <- which(country_code$region == x)
  country_code[row_index, "money_invested"] <- temp_sum
}

country_code$region[country_code$region == 'Russian Federation'] <- 'Russia'
country_code$region[country_code$region == 'Britain'] <- 'UK'
country_code

###GDP Data
gdp <- read_csv("C:/Users/Yan/Downloads/Results (1) - Results (1).csv")

gdp$`GDP, at current prices - US Dollars` <- gdp$`GDP, at current prices - US Dollars`/1000000
names(gdp)[1] <- "region"
names(gdp)[4] <- "GDP"
gdp <- select(gdp, -c("Year","Unit"))
country_code <- left_join(country_code, gdp, by="region")
country_code$percent_FDI_GDP <- country_code$money_invested / country_code$GDP     

View(country_code)
### Map Data
mapdata <- map_data("world")
mapdata <- left_join(mapdata, country_code, by="region")
map1<-ggplot(mapdata, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = money_invested), color = "black")
mapdata
map1


###Doing the same thing but in country years

dat_cyear <- read_csv("C:/Users/Yan/Downloads/China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx - Dataset 1+2.csv")
dat_cyear <- select(dat_cyear, -c("Investor", "Share Size", "Transaction Party", "Subsector", "Region", Greenfield, "Month"))
dat_cyear$CountryYear <- as.character(dat_cyear$Year)
dat_cyear$CountryYear <- paste(dat_cyear$Year, dat_cyear$Country)
dat_cyear[]<-lapply(dat_cyear,gsub,pattern="$",fixed=TRUE,replacement="")
dat_cyear[]<-lapply(dat_cyear,gsub,pattern=",",fixed=TRUE,replacement="")
dat_cyear$`Quantity in Millions` <- as.numeric(dat_cyear$`Quantity in Millions`)

countryyearlist <- c()
for(i in country_code$region){
  for(x in 2005:2022) {                     # Head of for-loop
    new_value <- paste(as.character(x), as.character(i))            # Creating new value
    countryyearlist <- c(countryyearlist, new_value)    # Appending new value to vector
  }
}

countryyear_code = data.frame(matrix(ncol = 2, nrow = length(c(countryyearlist))))
names(countryyear_code) = c("countryyear","money_invested")  
countryyear_code$countryyear = countryyearlist


for(x in countryyearlist){
  dat_temp<- dat_cyear %>% filter(dat_cyear$CountryYear == x)
  temp_sum <-sum(dat_temp[, "Quantity in Millions"])
  row_index <- which(countryyear_code$countryyear == x)
  countryyear_code[row_index, "money_invested"] <- temp_sum
}
write.csv(countryyear_code,"C:/Users/Yan/Documents/FDICountryYear.csv", row.names = FALSE)

