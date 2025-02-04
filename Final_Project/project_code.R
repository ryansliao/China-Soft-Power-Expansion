# Preliminaries
## loading packages
rm(list=ls(all=TRUE))
library(caret)
library(countrycode)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(kableExtra)
library(lubridate)
library(plyr)
library(rattle)
library(RColorBrewer)
library(readxl)
library(recipes)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(rsample)
library(sf)
library(tidyverse)
library(viridis)
library(yardstick)
library(zoo)

## setting working directory
setwd("C:/Users/jeffr/Downloads/POLI178/Final_Project/")

## loading raw data files
HDI <- read.csv("data/human-development-index.csv")
arms_import <- read_csv("data/china_arms_imports_tiv.csv")
arms_export <- read_csv("data/china_arms_exports_tiv.csv")
imports_full <- read_excel("data/WITS-Imports-Partner-Country.xlsx")
imports_SSA <- read_excel("data/WITS-Imports-Sub-Saharan-Africa.xlsx")
exports_full <- read_excel("data/WITS-Exports-Partner-Country.xlsx")
exports_SSA <- read_excel("data/WITS-Exports-Sub-Saharan-Africa.xlsx")
FDI <- read.csv("data/China-Global-Investment-Tracker-2022-SPRING-final-1.csv")
Fin_Dipl <- read_excel("data/ChineseFinancialPublicDiplomacyProjectDetails.xlsx")
Intl_Gov_Data <- read_excel("data/Government_Data.xls")
UN_Agree <- read.csv("data/AgreementScoresAll_Jun2022.csv")
IGO_Data <- read.csv("data/dyadic_formatv3.csv") # massive 1.19 GB file
polity <- read_excel("data/p5v2018.xls")
GDP <- read_excel("data/Download-GDPcurrent-USD-countries.xlsx")
resources <- read.csv("data/Total_Natural_Resources_Rents.csv", check.names = FALSE)

# Data Wrangling
## subsetting years to be after 2000
## also choosing just columns with year, country, and variable of interest
HDI <- HDI %>% filter(Year >= 2000)

arms_import <- select(arms_import, -c(as.character(1950:1999)))
arms_import <- arms_import[-15,] # remove extraneous empty line
arms_export <- select(arms_export, -c(as.character(1950:1999)))
arms_export <- arms_export[-99,] # remove extraneous empty line
imports <- select(imports_full, c("Partner Name", as.character(2000:2020)))
exports <- select(exports_full, c("Partner Name", as.character(2000:2020)))

UN_Agree <- select(UN_Agree, c("ccode1","ccode2","agree","year")) %>%
  subset(year>=2000) %>%
  subset(ccode1 == "710") # filtering for rows relating to China (ccode 710)
IGO_Data <- subset(IGO_Data, year>=2000)

polity <- select(polity, c("year","country","scode","polity2")) %>%
  filter(year >= 2000)

GDP <- select(GDP, c("Country", "IndicatorName",as.character(2000:2020)))

resources <- select(resources, c("Country Name", "Country Code",as.character(2000:2020)))

## Variable-Specific Wrangling
### Arms Imports/Exports
#### handle missingness by defaulting to 0 imports/exports 
arms_import[is.na(arms_import)] <- 0
arms_export[is.na(arms_export)] <- 0

#### rename country column
names(arms_import)[names(arms_import) == "...1"] <- "Country"
names(arms_export)[names(arms_export) == "...1"] <- "Country"

### Foreign Direct Investment
#### drop unnecessary columns
FDI <- select(FDI, -c("Investor", "Share.Size", "Transaction.Party", "Subsector", "Region", "Greenfield", "Month")) 

#### convert dollar amounts into numeric format
FDI[]<-lapply(FDI,gsub,pattern="$",fixed=TRUE,replacement="")
FDI[]<-lapply(FDI,gsub,pattern=",",fixed=TRUE,replacement="")
FDI$Quantity.in.Millions <- as.numeric(FDI$Quantity.in.Millions)

#### convert BRI column into numeric format
FDI$BRI <- as.numeric(FDI$BRI)

#### missingness treated as default 0 (not related to BRI)
FDI <- FDI %>% replace(is.na(.), 0)

#### create new dataframe for country resolution data (total investment over 2005-2022)
FDI_country_all_time <- data.frame(matrix(ncol = 2, nrow = length(unique(c(FDI$Country)))))
names(FDI_country_all_time) = c("Country","Total.Money.Invested")

#### setting column class types
class(FDI_country_all_time$Country) <- "Character"
FDI_country_all_time$Total.Money.Invested <- as.numeric(FDI_country_all_time$Total.Money.Invested)

#### put country names in
FDI_country_all_time$Country = unique(c(FDI$Country))

#### loop for all countries
for(x in FDI_country_all_time$Country){
  FDI_temp <- FDI %>% filter(FDI$Country == x)
  temp_sum <- sum(as.numeric(FDI_temp[, "Quantity.in.Millions"]))
  row_index <- which(FDI_country_all_time$Country == x)
  FDI_country_all_time[row_index, "Total.Money.Invested"] <- temp_sum
}

#### repeat for FDI by country and year
#### create new dataframe for country-year resolution data
FDI_country_year <- expand(tibble(), 2005:2022, FDI_country_all_time$Country)
FDI_country_year$Money.Invested <- 0
colnames(FDI_country_year) <- c("Year","Country","Money.Invested")  

for(x in FDI_country_all_time$Country){
  for(y in 2005:2022) {
    FDI_temp <- FDI %>% filter(FDI$Country == x & FDI$Year == y)
    temp_sum <- sum(as.numeric(FDI_temp[, "Quantity.in.Millions"]))
    row_index <- which(FDI_country_year$Country == x & FDI_country_year$Year == y)
    FDI_country_year[row_index, "Money.Invested"] <- temp_sum
  }
}

#### remove unnecessary variables
FDI <- FDI_country_year
rm(FDI_country_year, FDI_temp, row_index, temp_sum, x, y)

### Financial Diplomacy
#### convert number strings to numeric values
Fin_Dipl <- Fin_Dipl %>% 
  mutate_at(c("commitment_year", "implementation_start_year", "completion_year",
              "sector_code", "amount_original_currency", "amount_constant_usd2017",	
              "amount_nominal", "maturity", "interest_rate",	"grace_period", "management_fee",
              "commitment_fee",	"grant_element", "source_quality_score",	"data_completeness_score",
              "project_implementation_score",	"loan_detail_score"), as.numeric)

#### drop unnecessary columns
Fin_Dipl_full <- Fin_Dipl %>% select(-c("project_id", "recommended_for_research", "umbrella_project",
                                        "title", "description", "staff_comments", "receiving_agencies_origin",
                                        "implementing_agencies_origin", "accountable_agencies_origin",
                                        "planned_implementation_start_date", "planned_completion_date", 
                                        "official_source_count",	"unofficial_source_count", "source_urls",
                                        "source_titles",	"source_publishers",	"source_type", "location_details",
                                        "geojson_url_viz",	"geojson_url_dl", "contact_name",	"contact_position",
                                        "oda_eligible_recipient"))

#### converting data into country-year resolution
#### get relevant columns and remove rows without amount given
Fin_Dipl <- select(Fin_Dipl_full, c("recipient","commitment_year","amount_nominal"))
Fin_Dipl <- Fin_Dipl[!is.na(Fin_Dipl$amount_nominal), 1:3]

#### get list of all countries and years
Fin_Dipl_countries <- unique(Fin_Dipl$recipient)
Fin_Dipl_years <- 2000:2017
Fin_Dipl_country_year <- expand(tibble(), Fin_Dipl_years, Fin_Dipl_countries)
Fin_Dipl_country_year$amount <- NA

#### iterate and sum for total financial diplomatic investment in all countries and years
for(i in Fin_Dipl_countries){
  for(j in 2000:2017) {
    Fin_Dipl_subset <- Fin_Dipl %>% filter(Fin_Dipl$recipient == i & Fin_Dipl$commitment_year == j)
    country_year_sum <- sum(Fin_Dipl_subset[, 3])
    row_index <- which(Fin_Dipl_country_year$Fin_Dipl_countries == i & Fin_Dipl_country_year$Fin_Dipl_years == j)
    Fin_Dipl_country_year[row_index, 3] <- as.numeric(country_year_sum)
  }
}

#### rename columns
colnames(Fin_Dipl_country_year) <- c("Year", "Country", "Fin.Diplomacy")

#### remove unnecessary variables
Fin_Dipl <- Fin_Dipl_country_year
rm(Fin_Dipl_countries, Fin_Dipl_years, Fin_Dipl_country_year, Fin_Dipl_subset, country_year_sum, row_index)

### UN Agreement
#### matching UN_Agree country codes to country names
UN_Agree$ccode2 <- as.character(UN_Agree$ccode2) # recode to character

#### create df with ccode and country name conversion
country_code <- data.frame(Intl_Gov_Data$ccode, Intl_Gov_Data$country, Intl_Gov_Data$un_continent,
                           Intl_Gov_Data$un_region)
country_code <- distinct(country_code)
country_code <- country_code %>% rename(ccode2 = Intl_Gov_Data.ccode, 
                                        country = Intl_Gov_Data.country,
                                        un_continent = Intl_Gov_Data.un_continent,
                                        un_region = Intl_Gov_Data.un_region)

#### merge to get conversion between code and country
UN_Agree <- merge(UN_Agree, country_code, by = "ccode2")

### IGO Participation
#### select rows with CHN involved
China_IGO1 <- subset(IGO_Data, country1 == "CHN")
China_IGO2 <- subset(IGO_Data, country2 == "CHN")

#### reorder so China is always first country
China_IGO2 <- rename(China_IGO2, country1 = country2, country2 = country1, ccode1 = ccode2, ccode2 = ccode1)

#### combine to one big dataframe
China_IGO <- rbind(China_IGO1, China_IGO2)
China_IGO <- merge(China_IGO, country_code, by = "ccode2")

#### first subset for only IGO participation columns
IGO_Dummy <- China_IGO[c(7:540)]

#### handle missingness by defaulting to 0 (not participating)
IGO_Dummy <- replace(IGO_Dummy, IGO_Dummy < 0, 0)

#### calculate alignment score based on mutual participation
alignment <- rowSums(IGO_Dummy)
IGO <- data.frame(China_IGO, alignment)
IGO$alignment <- IGO$alignment/max(alignment)

#### remove unnecessary variables
rm(alignment, China_IGO, China_IGO1, China_IGO2, country_code, IGO_Data, IGO_Dummy, Intl_Gov_Data, i, j)

### GDP
#### subset for just GDP rows
GDP <- GDP %>% subset(IndicatorName == "Gross Domestic Product (GDP)")

## adding country codes to better match, manually if needed
arms_import$Code <- countrycode(arms_import$Country, "country.name", "iso3c")

arms_export$Code <- countrycode(arms_export$Country, "country.name", "iso3c")

imports$Code <- countrycode(imports$`Partner Name`, "country.name", "iso3c")
imports$Code[which(imports$'Partner Name' == "Czechoslovakia")] <- 'CSK'
imports$Code[which(imports$'Partner Name' == "Ethiopia(excludes Eritrea)")] <- 'ETH'
imports$Code[which(imports$'Partner Name' == "Netherlands Antilles")] <- 'ANT'
imports$Code[which(imports$'Partner Name' == "Occ.Pal.Terr")] <- 'PSE'
imports$Code[which(imports$'Partner Name' == "Serbia, FR(Serbia/Montenegro)")] <- 'SCG'

exports$Code <- countrycode(exports$`Partner Name`, "country.name", "iso3c")
exports$Code[which(exports$'Partner Name' == "Czechoslovakia")] <- 'CSK'
exports$Code[which(exports$'Partner Name' == "Ethiopia(excludes Eritrea)")] <- 'ETH'
exports$Code[which(exports$'Partner Name' == "Netherlands Antilles")] <- 'ANT'
exports$Code[which(exports$'Partner Name' == "Occ.Pal.Terr")] <- 'PSE'
exports$Code[which(exports$'Partner Name' == "Serbia, FR(Serbia/Montenegro)")] <- 'SCG'

FDI$Code <- countrycode(FDI$Country, "country.name", "iso3c")
FDI_country_all_time$Code <- countrycode(FDI_country_all_time$Country, "country.name", "iso3c")

Fin_Dipl$Code <- countrycode(Fin_Dipl$Country, "country.name", "iso3c")
Fin_Dipl$Code[which(Fin_Dipl$Country == "Micronesia")] <- 'FSM'

IGO$Code <- countrycode(IGO$country, "country.name", "iso3c")
IGO$Code[which(IGO$country == "Serbia and Montenegro")] <- 'SCG'
IGO$Code[which(IGO$country == "Yugoslavia")] <- 'YGS'

UN_Agree$Code <- countrycode(UN_Agree$country, "country.name", "iso3c")
UN_Agree$Code[which(UN_Agree$country == "Yugoslavia")] <- 'YGS'

polity$Code <- countrycode(polity$scode, "p4c", "iso3c")
polity$Code[which(polity$scode == "SUD")] <- 'SDN'
polity$Code[which(polity$scode == "YGS")] <- 'YUG'

GDP$Code <- countrycode(GDP$Country, "country.name", "iso3c")
GDP$Code[which(GDP$Country == "Czechoslovakia (Former)")] <- 'CSK'
GDP$Code[which(GDP$Country == "Former Netherlands Antilles")] <- 'ANT'
GDP$Code[which(GDP$Country == "Micronesia (FS of)")] <- 'FSM'
GDP$Code[which(GDP$Country == "Yemen Arab Republic (Former)")] <- 'YEM'
GDP$Code[which(GDP$Country == "Yugoslavia (Former)")] <- 'YUG'
GDP$Code[which(GDP$Country == "Zanzibar")] <- 'EAZ'

## standardizing dollar amounts
imports[, c(as.character(2000:2020))] <- imports[, c(as.character(2000:2020))] * 1000
exports[, c(as.character(2000:2020))] <- exports[, c(as.character(2000:2020))] * 1000
FDI$Money.Invested <- FDI$Money.Invested * 1000000

## pivoting to country-year resolution
arms_import <- pivot_longer(arms_import, as.character(2000:2021), names_to = "Year", values_to = "Arms.Imports")
arms_export <- pivot_longer(arms_export, as.character(2000:2021), names_to = "Year", values_to = "Arms.Exports")
imports <- pivot_longer(imports, as.character(2000:2020), names_to = "Year", values_to = "Imports")
exports <- pivot_longer(exports, as.character(2000:2020), names_to = "Year", values_to = "Exports")
GDP <- pivot_longer(GDP, as.character(2000:2020), names_to = "Year", values_to = "GDP")
resources <- pivot_longer(resources, as.character(2000:2020), names_to = "Year", values_to = "Resources")

arms_import$Year <- as.numeric(arms_import$Year)
arms_export$Year <- as.numeric(arms_export$Year)
imports$Year <- as.numeric(imports$Year)
exports$Year <- as.numeric(exports$Year)
GDP$Year <- as.numeric(GDP$Year)
resources$Year <- as.numeric(resources$Year)

## getting totals for each country in arms imports/exports
arms_import_w_total <- arms_import[!is.na(arms_import$Code),] %>% 
  select(-c("Year","Arms.Imports")) %>% 
  distinct()
arms_export_w_total <- arms_export[!is.na(arms_export$Code),] %>% 
  select(-c("Year","Arms.Exports")) %>% 
  distinct()

## removing no-match rows and renaming columns
HDI <- HDI[HDI$Code != "",]
colnames(HDI) <- c("Country", "Code", "Year", "HDI")

imports <- imports[!is.na(imports$Code),]
exports <- exports[!is.na(exports$Code),]

colnames(FDI) <- c("Year", "Country", "FDI", "Code")

UN_Agree <- UN_Agree[!is.na(UN_Agree$Code),c("agree","year","country","Code")] # selecting relevant portions
colnames(UN_Agree) <- c("UN.Agreement", "Year", "Country", "Code")
IGO <- IGO[!is.na(IGO$Code),c("year","country","alignment","Code")] # selecting relevant portions
colnames(IGO) <- c("Year", "Country", "IGO.Alignment", "Code")

polity <- select(polity, -scode)
polity <- polity[!is.na(polity$Code),]
colnames(polity) <- c("Year", "Country", "Polity", "Code")

colnames(GDP) <- c("Country", "Indicator.Name", "Code", "Year", "GDP")

resources <- resources[!is.na(resources$`Country Code`),]
colnames(resources) <- c("Country", "Code", "Year", "Resources")

## reduce all variables to have just year, code, and variable
HDI <- select(HDI, c("Year", "Code", "HDI"))
arms_import <- select(arms_import, c("Year", "Code", "Arms.Imports"))
arms_export <- select(arms_export, c("Year", "Code", "Arms.Exports"))
imports <- select(imports, c("Year", "Code", "Imports"))
exports <- select(exports, c("Year", "Code", "Exports"))
FDI <- select(FDI, c("Year", "Code", "FDI"))
Fin_Dipl <- select(Fin_Dipl, c("Year", "Code", "Fin.Diplomacy"))
IGO <- select(IGO, c("Year", "Code", "IGO.Alignment"))
UN_Agree <- select(UN_Agree, c("Year", "Code", "UN.Agreement"))
polity <- select(polity, c("Year", "Code", "Polity"))
GDP <- select(GDP, c("Year", "Code", "GDP"))
resources <- select(resources, c("Year", "Code", "Resources"))

## merge time
merge_list <- list(HDI, arms_import, arms_export, imports, exports, FDI, Fin_Dipl, 
                   IGO, UN_Agree, polity, GDP, resources)
df <- merge_list %>% reduce(full_join, by=c("Year", "Code"))
df$Year <- as.character(df$Year)

## save df to save computation time
write.csv(df, "output/FINAL_DATASET.csv", row.names = FALSE)

# Data Exploration
## HDI Score
### get 2021 data and rank them
HDI_2021 <- HDI %>% filter(Year == "2021")
HDI_2021$rank <- rank(-HDI_2021$HDI, ties.method = "first")

### Scatterplot
jpeg('figures/HDI_vs_Rank.jpg', width = 900, height = 600, pointsize = 16, quality = 100)
plot(x = HDI_2021$rank, y = HDI_2021$HDI,
     xlab = "OVERALL RANK", ylab = "HDI SCORE", col = "green",
     pch = 4, main = "HDI SCORE vs. RANK (Scatterplot)")

abline(lm(HDI ~ rank, data = HDI_2021), col = "black")
dev.off()

### Geospatial: Africa
#### first get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

#### then just Africa
AFR_sf <- world %>% filter(region_un == "Africa")

AFR_sf <- merge(AFR_sf, HDI_2021, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

jpeg('figures/Africa_HDI.jpg', width = 800, height = 600, pointsize = 16, quality = 100)
ggplot(data=AFR_sf, aes(fill = HDI)) + 
  scale_fill_gradientn(colours = c("#132B43", "#56B1F7"),
                       name = "Human Development Index") +
  geom_sf() + ggtitle("Human Development Index in Africa")
dev.off()

### Geospatial: South America
#### now get just South America
SA_sf <- world %>% filter(continent == 'South America')

SA_sf <- merge(SA_sf, HDI_2021, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

jpeg('figures/South_America_HDI.jpg', width = 800, height = 600, pointsize = 16, quality = 100)
ggplot(data=SA_sf, aes(fill = HDI)) +
  scale_fill_gradientn(colours = c("#132B43", "#56B1F7"),
                       name = "Human Development Index") +
  geom_sf() + ggtitle("Human Development Index in South America")
dev.off()

### Geospatial: Middle East
#### now get just Middle East
ME_sf <- world %>% filter(subregion == 'Western Asia')

ME_sf <- merge(ME_sf, HDI_2021, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

jpeg('figures/Middle_East_HDI.jpg', width = 800, height = 600, pointsize = 16, quality = 100)
ggplot(data=ME_sf, aes(fill = HDI)) +
  scale_fill_gradientn(colours = c("#132B43", "#56B1F7"),
                       name = "Human Development Index") +
  geom_sf() + ggtitle("Human Development Index in the Middle East")
dev.off()

## Trade: Arms Imports
### get dataframe with arms imports
arms_import_geo <- merge(world, arms_import_w_total, by.x = "iso_a3", by.y = "Code", all=TRUE)

### missing data assumed to mean no arms imports
arms_import_geo[is.na(arms_import_geo$Total),"Total"] <- 0

### geospatial
jpeg('figures/Arms_Imports.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data = arms_import_geo) +
  geom_sf(aes(fill = Total)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "Chinese Arms Imports \n2000-2021", fill = "Total Arms Imports \n(US$ Millions)")
dev.off()

### use linear regression to see trend in arms imports
arms_import_models <- dlply(arms_import, "Code", function(df) 
  lm(Arms.Imports ~ Year, data = df))
arms_import_coefficients <- ldply(arms_import_models, coef)

arms_import_reg_geo <- merge(world, arms_import_coefficients, by.x = "iso_a3", by.y = "Code", all=TRUE)
arms_import_reg_geo[is.na(arms_import_reg_geo)] <- 0

jpeg('figures/Arms_Imports_Growth.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data = arms_import_reg_geo) + 
  geom_sf(aes(fill = Year)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Chinese Arms Imports Growth \n2000-2021", 
       fill = "Average Change in \nArms Imports Per Year\n(US$ Millions/Year)")
dev.off()

## Trade: Arms Exports
### get dataframe with arms exports
arms_export_geo <- merge(world, arms_export_w_total, by.x = "iso_a3", by.y = "Code", all=TRUE)

### missing data assumed to mean no arms exports
arms_export_geo[is.na(arms_export_geo$Total),"Total"] <- 0

### geospatial
jpeg('figures/Arms_Exports.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data = arms_export_geo) +
  geom_sf(aes(fill = Total)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "Chinese Arms Exports \n2000-2021", fill = "Total Arms Exports \n(US$ Millions)")
dev.off()

### use linear regression to see trend in arms exports
arms_export_models <- dlply(arms_export, "Code", function(df) 
  lm(Arms.Exports ~ Year, data = df))
arms_export_coefficients <- ldply(arms_export_models, coef)

arms_export_reg_geo <- merge(world, arms_export_coefficients, by.x = "iso_a3", by.y = "Code", all=TRUE)
arms_export_reg_geo[is.na(arms_export_reg_geo)] <- 0

jpeg('figures/Arms_Exports_Growth.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data = arms_export_reg_geo) + 
  geom_sf(aes(fill = Year)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Chinese Arms Exports Growth \n2000-2021", 
       fill = "Average Change in \nArms Exports Per Year \n(US$ Millions/Year)")
dev.off()

## Trade: Imports
### get total imports by year
imports_year <- colSums(imports_full[, as.character(1992:2020)], na.rm = TRUE)
imports_year <- data.frame(1992:2020, imports_year/10^9)
colnames(imports_year) <- c("Year", "Total.Imports")

jpeg('figures/Total_Imports.jpg', width = 900, height = 600, pointsize = 16, quality = 100)
ggplot(imports_year, aes(x = Year, y = Total.Imports))+
  geom_line(aes(group=1)) +
  geom_point() +
  labs(title = "Total Chinese Imports", x = "Year", y = "Imports (US$ Trillions)")
dev.off()

### Sub-Saharan Africa imports
#### get important columns
imports_SSA <- select(imports_SSA, c("Product Group", as.character(1992:2020)))

#### pivot for graphing
imports_SSA <- imports_SSA %>% 
  pivot_longer(!`Product Group`, names_to = "Year", values_to = "Imports")

jpeg('figures/Sub-Saharan_Africa_Imports.jpg', width = 1000, height = 500, pointsize = 16, quality = 100)
ggplot(imports_SSA, aes(x = Year, y = Imports/10^6, 
                        color = `Product Group`, group = `Product Group`)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Chinese Imports to Sub-Saharan Africa", x = "Year", 
       y = "Imports (US$ Billions)", color = "Product Group")
dev.off()

## Trade: Exports
### get total exports by year
exports_year <- colSums(exports_full[, as.character(1992:2020)], na.rm = TRUE)
exports_year <- data.frame(1992:2020, exports_year/10^9)
colnames(exports_year) <- c("Year", "Total.Exports")

jpeg('figures/Total_Exports.jpg', width = 900, height = 600, pointsize = 16, quality = 100)
ggplot(exports_year, aes(x = Year, y = Total.Exports))+
  geom_line(aes(group=1)) +
  geom_point() +
  labs(title = "Total Chinese Exports", x = "Year", y = "Exports (US$ Trillions)")
dev.off()

### Sub-Saharan Africa exports
#### get important columns
exports_SSA <- select(exports_SSA, c("Product Group", as.character(1992:2020)))

#### pivot for graphing
exports_SSA <- exports_SSA %>% 
  pivot_longer(!`Product Group`, names_to = "Year", values_to = "Exports")

jpeg('figures/Sub-Saharan_Africa_Exports.jpg', width = 1000, height = 500, pointsize = 16, quality = 100)
ggplot(exports_SSA, aes(x = Year, y = Exports/10^6, 
                        color = `Product Group`, group = `Product Group`)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Chinese Exports to Sub-Saharan Africa", x = "Year", 
       y = "Exports (US$ Billions)", color = "Product Group")
dev.off()

## Foreign Direct Investment
### normalize FDI by current GDP
GDP_2020 <- subset(GDP, Year == "2020")
GDP_2020 <- GDP_2020[!is.na(GDP_2020$GDP),]
FDI_country_all_time <- left_join(FDI_country_all_time, GDP_2020, by = "Code")
FDI_country_all_time$Normalized.FDI <- FDI_country_all_time$Total.Money.Invested / FDI_country_all_time$GDP * 10^6    

### geospatial, unnormalized
FDI_geo <- merge(world, FDI_country_all_time, by.x = "iso_a3", by.y = "Code", all.x = TRUE)
jpeg('figures/Total_FDI.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data = FDI_geo) +
  geom_sf(aes(fill = Total.Money.Invested)) +
  scale_fill_viridis_c(option = "rocket", trans = "sqrt", direction = -1) +
  labs(title = "Total Foreign Direct Investment \n2005-2022", 
       fill = "Total FDI \n(US$ Millions)")
dev.off()

### geospatial, normalized
jpeg('figures/Total_FDI_by_GDP.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data = FDI_geo) +
  geom_sf(aes(fill = Normalized.FDI)) +
  scale_fill_viridis_c(option = "rocket", trans = "sqrt", direction = -1) +
  labs(title = "Total Foreign Direct Investment, Normalized by GDP \n2005-2022", 
       fill = "Total FDI Divided \nby GDP")
dev.off()

## Financial Diplomacy
### visualize financial diplomacy
#### bar graph of how many times a country has received financial diplomacy
ggplot(Fin_Dipl_full, aes(x=recipient)) +
  geom_bar()

#### histogram of project implementation scores
hist(Fin_Dipl_full$project_implementation_score, breaks = 0.5:5.5)

#### histogram of how complete the data are
hist(Fin_Dipl_full$data_completeness_score, breaks = -0.5:5.5)

#### histogram of the quality of the source
hist(Fin_Dipl_full$source_quality_score, breaks = 0.5:5.5)

#### histogram of how detailed the loan information is
hist(Fin_Dipl_full$loan_detail_score, breaks = 0.5:5.5)

jpeg('figures/Financial_Diplomacy_Types.jpg', width = 900, height = 800, pointsize = 16, quality = 100)
ggplot(Fin_Dipl_full, aes(x=flow_type)) +
  labs(title = "Histogram of Types of Financial Support", x = "Types", 
       y = "Frequency") +
  geom_bar() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))
dev.off()

jpeg('figures/Financial_Diplomacy_Intent.jpg', width = 900, height = 600, pointsize = 16, quality = 100)
ggplot(Fin_Dipl_full, aes(x=intent)) +
  labs(title = "Histogram of Intent of Financial Investment", x = "Intent", 
       y = "Frequency") +
  geom_bar() +
  theme_light()
dev.off()

jpeg('figures/Financial_Diplomacy_Region.jpg', width = 900, height = 600, pointsize = 16, quality = 100)
ggplot(Fin_Dipl_full, aes(x=recipient_region)) +
  labs(title = "Histogram of Financial Diplomacy by Region", x = "Region", 
       y = "Frequency") +
  geom_bar() +
  theme_light()
dev.off()

##### seems there are a few countries that have gotten the most from China

##### most frequent country
names(which.max(table(Fin_Dipl_full$recipient)))

##### Angola is the most frequent

##### most frequently invested sector
names(which.max(table(Fin_Dipl_full$sector_name)))

##### education is the most frequently invested

##### top 10 most frequent countries
sort(table(Fin_Dipl_full$recipient), decreasing = TRUE)[1:10]

##### the top 10 countries are Angola, Cambodia, Myanmar, Sudan, Pakistan, 
#####     Indonesia, DR Congo, Laos, Ethiopia, and Tanzania.

##### top 10 most frequent sectors
sort(table(Fin_Dipl_full$sector_name), decreasing = TRUE)[1:10]

## Diplomacy: UN Agreement
### subset for specific Africa and South America regions
UN_geo <- merge(world, UN_Agree, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

UN_geo_Africa <- subset(UN_geo, continent == "Africa")
UN_geo_SAmerica <- subset(UN_geo, continent == "South America")

UN_geo_WAfrica <- subset(UN_geo_Africa, subregion == "Western Africa")
UN_geo_SAfrica <- subset(UN_geo_Africa, subregion == "Southern Africa")
UN_geo_MAfrica <- subset(UN_geo_Africa, subregion == "Middle Africa")
UN_geo_EAfrica <- subset(UN_geo_Africa, subregion == "Eastern Africa")
UN_geo_NAfrica <- subset(UN_geo_Africa, subregion == "Northern Africa")

### large color palette creation
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

### line graph of UN agreement over time for Africa
ggplot(data = UN_geo_Africa, aes(x = Year, y = UN.Agreement, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2021, 2)) +
  labs(title = "UN Voting Agreement Levels Across Africa", x = "Year", y = "Agreement")

### repeat for Western Africa
WAfrica_UN <- ggplot(data = UN_geo_WAfrica, aes(x = Year, y = UN.Agreement, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2021, 2)) +
  labs(title = "UN Voting Agreement Levels in Western Africa", x = "Year", y = "Agreement")

### repeat for Southern Africa
SAfrica_UN <- ggplot(data = UN_geo_SAfrica, aes(x = Year, y = UN.Agreement, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2021, 2)) +
  labs(title = "UN Voting Agreement Levels in Southern Africa", x = "Year", y = "Agreement")

### repeat for Middle Africa
MAfrica_UN <- ggplot(data = UN_geo_MAfrica, aes(x = Year, y = UN.Agreement, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2021, 2)) +
  labs(title = "UN Voting Agreement Levels in Middle Africa", x = "Year", y = "Agreement")

### repeat for Eastern Africa
EAfrica_UN <- ggplot(data = UN_geo_EAfrica, aes(x = Year, y = UN.Agreement, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2021, 2)) +
  labs(title = "UN Voting Agreement Levels in Eastern Africa", x = "Year", y = "Agreement")

### repeat for Northern Africa
NAfrica_UN <- ggplot(data = UN_geo_NAfrica, aes(x = Year, y = UN.Agreement, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2021, 2)) +
  labs(title = "UN Voting Agreement Levels in Northern Africa", x = "Year", y = "Agreement")

### tile together the African subregions
jpeg('figures/Africa_UN.jpg', width = 1500, height = 900, pointsize = 16, quality = 100)
grid.arrange(WAfrica_UN, SAfrica_UN, MAfrica_UN, EAfrica_UN, NAfrica_UN, nrow = 2)
dev.off()

### repeat for South America
jpeg('figures/South_America_UN.jpg', width = 900, height = 600, pointsize = 16, quality = 100)
ggplot(data = UN_geo_SAmerica, aes(x = Year, y = UN.Agreement, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2021, 2)) +
  labs(title = "UN Voting Agreement Levels Across South America", x = "Year", y = "Agreement")
dev.off()

### now to look at analyses
### averages across all years for each country in South America in new dataframe
avg_SAmerica_UN <- UN_geo_SAmerica  %>%
  st_drop_geometry() %>%
  group_by(name) %>%
  summarise_at(vars(UN.Agreement), list(name = mean))
avg_SAmerica_UN <- data.frame(unique(UN_geo_SAmerica$name), avg_SAmerica_UN)
colnames(avg_SAmerica_UN) <- c("Country", "Average Agreement")
avg_SAmerica_UN[!is.na(avg_SAmerica_UN$`Average Agreement`),]

### averages across all years for each country in Africa in new dataframe
avg_Africa_UN <- UN_geo_Africa  %>%
  st_drop_geometry() %>% 
  group_by(name) %>%
  summarise_at(vars(UN.Agreement), list(name = mean))
avg_Africa_UN <- data.frame(unique(UN_geo_Africa$name), avg_Africa_UN)
colnames(avg_Africa_UN) <- c("Country", "Average Agreement")
avg_Africa_UN[!is.na(avg_Africa_UN$`Average Agreement`),]

## Diplomacy: IGO Participation
### subset for specific Africa and South America regions
IGO_geo <- merge(world, IGO, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

IGO_geo_Africa <- subset(IGO_geo, continent == "Africa")
IGO_geo_SAmerica <- subset(IGO_geo, continent == "South America")

IGO_geo_WAfrica <- subset(IGO_geo_Africa, subregion == "Western Africa")
IGO_geo_SAfrica <- subset(IGO_geo_Africa, subregion == "Southern Africa")
IGO_geo_MAfrica <- subset(IGO_geo_Africa, subregion == "Middle Africa")
IGO_geo_EAfrica <- subset(IGO_geo_Africa, subregion == "Eastern Africa")
IGO_geo_NAfrica <- subset(IGO_geo_Africa, subregion == "Northern Africa")

### Western Africa and China IGO Alignment
ggplot(IGO_geo_WAfrica, aes(x = Year, y = IGO.Alignment, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2014, 2)) +
  labs(title = "IGO Alignment in Western Africa", x = "Year", y = "Degree of Alignment")

### Southern Africa and China IGO Alignment
ggplot(IGO_geo_SAfrica, aes(x = Year, y = IGO.Alignment, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2014, 2)) +
  labs(title = "IGO Alignment in Southern Africa", x = "Year", y = "Degree of Alignment")

### Middle Africa and China IGO Alignment
ggplot(IGO_geo_MAfrica, aes(x = Year, y = IGO.Alignment, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2014, 2)) +
  labs(title = "IGO Alignment in Middle Africa", x = "Year", y = "Degree of Alignment")

### Eastern Africa and China IGO Alignment
ggplot(IGO_geo_EAfrica, aes(x = Year, y = IGO.Alignment, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2014, 2)) +
  labs(title = "IGO Alignment in Eastern Africa", x = "Year", y = "Degree of Alignment")

### North Africa and China IGO Alignment
ggplot(IGO_geo_NAfrica, aes(x = Year, y = IGO.Alignment, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2014, 2)) +
  labs(title = "IGO Alignment in Northern Africa", x = "Year", y = "Degree of Alignment")

### South America and China IGO Alignment
jpeg('figures/South_America_IGO.jpg', width = 900, height = 600, pointsize = 16, quality = 100)
ggplot(IGO_geo_SAmerica, aes(x = Year, y = IGO.Alignment, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2014, 2)) +
  labs(title = "IGO Alignment in South America", x = "Year", y = "Degree of Alignment")
dev.off()

### Africa and China IGO Alignment
jpeg('figures/Africa_IGO.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(IGO_geo_Africa, aes(x = Year, y = IGO.Alignment, color = name)) +
  geom_line() +
  scale_color_manual(values = col_vector, name = "Country") +
  scale_x_continuous(breaks=seq(2000, 2014, 2)) +
  labs(title = "IGO Alignment in Africa", x = "Year", y = "Degree of Alignment")
dev.off()

### now to look at analyses
### averages across all years for each country in South America in new dataframe
avg_SAmerica_IGO <- IGO_geo_SAmerica  %>%
  st_drop_geometry() %>%
  group_by(name) %>%
  summarise_at(vars(IGO.Alignment), list(name = mean))
avg_SAmerica_IGO <- data.frame(unique(IGO_geo_SAmerica$name), avg_SAmerica_IGO)
colnames(avg_SAmerica_IGO) <- c("Country", "Average Alignment")
avg_SAmerica_IGO[!is.na(avg_SAmerica_IGO$`Average Alignment`),]

### averages across all years for each country in Africa in new dataframe
avg_Africa_IGO <- IGO_geo_Africa  %>%
  st_drop_geometry() %>%
  group_by(name) %>%
  summarise_at(vars(IGO.Alignment), list(name = mean))
avg_Africa_IGO <- data.frame(unique(IGO_geo_Africa$name), avg_Africa_IGO)
colnames(avg_Africa_IGO) <- c("Country", "Average Alignment")
avg_Africa_IGO[!is.na(avg_Africa_IGO$`Average Alignment`),]

## Polity
### look at table for summary of polity for 2000 to 2018
table(polity$Polity)
sum(is.na(polity$Polity))

### histogram
hist(polity$Polity, breaks = -10.5:10.5, xlab = 'Polity', ylab = 'Frequency', 
     main = 'Histogram of Polity Types \n2000 to 2018')

### now look at polity in 2018
polity2018 <- subset(polity, Year == 2018)
table(polity2018$Polity)
sum(is.na(polity2018$Polity))
hist(polity2018$Polity, breaks = -10.5:10.5, xlab = 'Polity', ylab = 'Frequency', 
     main = 'Histogram of Polity Types \n2018')

### geospatial in 2018
polity_geo <- merge(world, polity2018, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

jpeg("figures/Polity_World.jpg", width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data=polity_geo, aes(fill = Polity)) + 
  scale_fill_gradientn(colours = c("red", "white", "blue"),
                       name = "Polity Score") +
  geom_sf() + ggtitle("Global Regime Type in 2018")
dev.off()

## GDP
### look at GDP in 2020
jpeg("figures/GDP_Histogram.jpg", width = 900, height = 600, pointsize = 16, quality = 100)
hist(GDP_2020$GDP, breaks = 100, xlab = 'GDP (US$)', ylab = 'Countries', 
     main = 'Histogram of GDP \n2020')
dev.off()

### most countries clustered strongly to the left, 
### but there are a handful of very productive countries

### geospatial
GDP_geo <- merge(world, GDP_2020, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

jpeg("figures/GDP_World.jpg", width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data=GDP_geo, aes(fill = GDP)) + 
  scale_fill_gradientn(colours = c("brown", "gray95", "orange"), trans = "log",
                       name = "GDP (US$)") +
  geom_sf() + ggtitle("GDP Across the World")
dev.off()

## Resources
### look at resources (% of GDP) in 2020
resources_2020 <- resources %>% subset(Year == 2020)

jpeg("figures/Resources_Histogram.jpg", width = 900, height = 600, pointsize = 16, quality = 100)
hist(resources_2020$Resources, xlab = 'Natural Resources Rents (% of GDP)', ylab = 'Countries', 
     main = 'Histogram of Natural Resources Rents, Normalized by GDP \n2020')
dev.off()

### geospatial
resources_geo <- merge(world, resources_2020, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

jpeg("figures/Resources_World.jpg", width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data=resources_geo, aes(fill = Resources)) + 
  scale_fill_gradientn(colours = c("blue", "white", "darkgreen"), trans = "sqrt",
                       name = "Natural Resources \n Rents (% of GDP)") +
  geom_sf() + ggtitle("Share of Economy Natural Resources Accounts for Across the World")
dev.off()

# Competitive ML
## Preliminaries
### setting seed
set.seed(42)

### dealing with missingness in data
#### rule of thumb: if GDP doesn't exist, data are likely insufficient
df <- df[!is.na(df$GDP),]

#### polity only lasts to 2018, but we will assume the polity stays the same to 2020
country_list <- unique(df$Code)
for (i in country_list) {
  polity2018 <- df$Polity[which(df$Code == i & df$Year == "2018")]
  df$Polity[which(df$Code == i & (df$Year == "2019" | df$Year == "2020"))] <- polity2018
}

#### missing data for arms imports, arms exports, imports, and exports can be considered $0
#### assume missing data for resources means 0 of GDP is accounted by natural resources
#### assume missing polity suggests intervention or uncharacterizable regime,
####    thus neither democratic nor autocratic, 0 for polity
columns_to_fill <- c("Arms.Imports","Arms.Exports","Imports","Exports","Resources","Polity")
df[,columns_to_fill] <- df[,columns_to_fill] %>% replace(is.na(.), 0)

#### arms data is too militarily based; remove
#### IGO alignment is too sparse, difficult to interpolate; remove
#### financial diplomacy only lasts to 2017, hard to make future predictions; remove
df <- df %>% select(-c("Arms.Imports","Arms.Exports","IGO.Alignment","Fin.Diplomacy"))

### data for two models
#### one for predicting FDI (2005-2020), one for UN agreement (2000-2020)
#### difficult to interpolate HDI, drop rows with NA HDI
df_econ <- df %>% filter(Year >= 2005 & Year < 2020) %>% select(-"UN.Agreement")
df_econ <- df_econ[!(is.na(df_econ$FDI) | is.na(df_econ$HDI)),]

df_dipl <- df %>% filter(Year < 2020) %>% select(-"FDI")
df_dipl <- df_dipl[!(is.na(df_dipl$UN.Agreement) | is.na(df_dipl$HDI)),]

### splitting data, 80-20 split
econ_splitter = initial_split(df_econ, prop = 0.8)
econ_train_data = training(econ_splitter)
econ_test_data = testing(econ_splitter)

dipl_splitter = initial_split(df_dipl, prop = 0.8)
dipl_train_data = training(dipl_splitter)
dipl_test_data = testing(dipl_splitter)

### recipe pre-processing
econ_rcp <- recipe(FDI~.,data = econ_train_data) %>% 
  step_dummy(all_nominal(),-FDI) %>%
  step_range(all_numeric(),-FDI) %>%
  prep()

dipl_rcp <- recipe(UN.Agreement~.,data = dipl_train_data) %>% 
  step_dummy(all_nominal(),-UN.Agreement) %>% 
  step_range(all_numeric(),-UN.Agreement) %>%
  prep()

### bake with recipes
baked_econ_train_data <- bake(econ_rcp,econ_train_data)
baked_econ_test_data <- bake(econ_rcp,econ_test_data)

baked_dipl_train_data <- bake(dipl_rcp,dipl_train_data)
baked_dipl_test_data <- bake(dipl_rcp,dipl_test_data)

### use 10 k-folds
econ_folds <- createFolds(baked_econ_train_data$FDI, k = 10)
dipl_folds <- createFolds(baked_dipl_train_data$UN.Agreement, k = 10)

### validation conditions
econ_val_cond <- trainControl(method='cv', index = econ_folds)
dipl_val_cond <- trainControl(method='cv', index = dipl_folds)

### Linear Regression
econ_mod_lm <- train(FDI ~ ., data=baked_econ_train_data, method = "lm", 
                     metric = "RMSE", trControl = econ_val_cond)

dipl_mod_lm <- train(UN.Agreement ~ ., data=baked_dipl_train_data, method = "lm", 
                     metric = "RMSE", trControl = dipl_val_cond)


### KNN
econ_mod_knn <- train(FDI ~ ., data=baked_econ_train_data, method = "knn", 
                      metric = "RMSE", trControl = econ_val_cond)

dipl_mod_knn <- train(UN.Agreement ~ ., data=baked_dipl_train_data, method = "knn", 
                     metric = "RMSE", trControl = dipl_val_cond)

#### plot KNN RMSE
plot(econ_mod_knn)
plot(dipl_mod_knn)

#### try lower k = 1:7
knn_tune = expand.grid(k = 1:7)
econ_mod_knn2 <- train(FDI ~ ., data=baked_econ_train_data, method = "knn",
                       metric = "RMSE", trControl = econ_val_cond, tuneGrid = knn_tune)

dipl_mod_knn2 <- train(UN.Agreement ~ ., data=baked_dipl_train_data, method = "knn", 
                       metric = "RMSE", trControl = dipl_val_cond, tuneGrid = knn_tune)

#### plot tuned KNN RMSE
plot(econ_mod_knn2) # minimum at k = 7
plot(dipl_mod_knn2) # minimum at k = 3

### Decision Trees
econ_mod_cart <- train(FDI ~ ., data=baked_econ_train_data, method = "rpart",
                       metric = "RMSE", trControl = econ_val_cond)

dipl_mod_cart <- train(UN.Agreement ~ ., data=baked_dipl_train_data, method = "rpart", 
                      metric = "RMSE", trControl = dipl_val_cond)

#### plot decision trees RMSE
plot(econ_mod_cart)
plot(dipl_mod_cart)

#### try lower alpha value
tune_cart <- expand.grid(cp = c(0.003, 0.01, 0.03, 0.08, 0.1, 0.12))

econ_mod_cart2 <- train(FDI ~ ., data=baked_econ_train_data, method = "rpart",
                       metric = "RMSE", trControl = econ_val_cond, tuneGrid = tune_cart)

dipl_mod_cart2 <- train(UN.Agreement ~ ., data=baked_dipl_train_data, method = "rpart", 
                       metric = "RMSE", trControl = dipl_val_cond, tuneGrid = tune_cart)

plot(econ_mod_cart2) # minimum at alpha = 0.12
plot(dipl_mod_cart2) # minimum at alpha = 0.01

#### plot decision trees
fancyRpartPlot(econ_mod_cart2$finalModel)
fancyRpartPlot(dipl_mod_cart2$finalModel)

### Random Forest
econ_mod_rf <- train(FDI ~ ., data=baked_econ_train_data, method = "ranger",
                       metric = "RMSE", trControl = econ_val_cond)

dipl_mod_rf <- train(UN.Agreement ~ ., data=baked_dipl_train_data, method = "ranger", 
                       metric = "RMSE", trControl = dipl_val_cond)

plot(econ_mod_rf) # minimum at 87
plot(dipl_mod_rf) # minimum at 184

### Comparison
econ_mod_list <- list(lm = econ_mod_lm, knn = econ_mod_knn2, 
                      cart = econ_mod_cart2, rf = econ_mod_rf)

dipl_mod_list <- list(lm = dipl_mod_lm, knn = dipl_mod_knn2,
                      cart = dipl_mod_cart2, rf = dipl_mod_rf)

#### resampling
dotplot(resamples(econ_mod_list),metric = "Rsquared") # random forest does best

jpeg('figures/economic_models_dotplot.jpg', width = 600, height = 400, pointsize = 16, quality = 100)
dotplot(resamples(econ_mod_list),metric = "RMSE") # random forest does best
dev.off()

dotplot(resamples(dipl_mod_list),metric = "Rsquared") # random forest does best

jpeg('figures/diplomatic_models_dotplot.jpg', width = 600, height = 400, pointsize = 16, quality = 100)
dotplot(resamples(dipl_mod_list),metric = "RMSE") # random forest does best
dev.off()

#### input testing data
econ_pred_rf <- predict(econ_mod_rf, newdata = baked_econ_test_data)
dipl_pred_rf <- predict(dipl_mod_rf, newdata = baked_dipl_test_data)

#### compare actual to prediction
econ_perf <- tibble(truth=baked_econ_test_data$FDI, estimate = econ_pred_rf)
dipl_perf <- tibble(truth=baked_dipl_test_data$UN.Agreement, estimate = dipl_pred_rf)

econ_perf_table <- bind_rows(econ_perf %>% rmse(truth,estimate), econ_perf %>% rsq(truth,estimate))
dipl_perf_table <- bind_rows(dipl_perf %>% rmse(truth,estimate), dipl_perf %>% rsq(truth,estimate))

### Prediction Using 2020 Data
#### get data
df_2020 <- df %>% filter(Year == 2020) %>% select(-c("UN.Agreement","FDI"))

#### bake 2020 data
baked_econ_2020 <- bake(econ_rcp,df_2020) %>% replace(is.na(.),0)
baked_dipl_2020 <- bake(dipl_rcp,df_2020) %>% replace(is.na(.),0)

#### predict
econ_pred_2020 <- predict(econ_mod_rf, baked_econ_2020)
dipl_pred_2020 <- predict(dipl_mod_rf, baked_dipl_2020)
df_2020$FDI.Pred <- econ_pred_2020
df_2020$UN.Agreement.Pred <- dipl_pred_2020

#### set China to NA (doesn't make sense to have soft power with itself)
df_2020$FDI.Pred[which(df_2020$Code == "CHN")] <- NA
df_2020$UN.Agreement.Pred[which(df_2020$Code == "CHN")] <- NA

#### normalize FDI with GDP
df_2020$FDI.Norm.Pred <- df_2020$FDI.Pred/df_2020$GDP

### Geospatial with Predictions
world_pred <- merge(world, df_2020, by.x = "iso_a3", by.y = "Code", all.x = TRUE)

jpeg('figures/Predicted_FDI_Expansion_2020.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data=world_pred, aes(fill = FDI.Pred)) + 
  scale_fill_gradientn(limits = c(1*10^6,15*10^10),
                       colours = c("white", "pink", "purple"),
                       trans = "log",
                       name = "Predicted FDI Expansion, US$") +
  geom_sf() + ggtitle("Predicted FDI Expansion, US$")
dev.off()

#### for better visualization of differences, cut off FDI.Norm.Pred > 0.1 to just 0.1
world_pred$FDI.Norm.Pred <- ifelse(world_pred$FDI.Norm.Pred > 0.1, 0.1, world_pred$FDI.Norm.Pred)

jpeg('figures/Predicted_FDI_Expansion_Normalized_2020.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data=world_pred, aes(fill = FDI.Norm.Pred)) + 
  scale_fill_gradientn(limits = c(0,0.1),
                       colours = c("white", "gold"),
                       trans = "sqrt",
                       name = "Predicted FDI Expansion, \nNormalized by GDP") +
  geom_sf() + ggtitle("Predicted FDI Expansion, Normalized by GDP")
dev.off()

jpeg('figures/Predicted_UN_Agreement_2020.jpg', width = 1200, height = 600, pointsize = 16, quality = 100)
ggplot(data=world_pred, aes(fill = UN.Agreement.Pred)) +
  scale_fill_gradientn(limits = c(0.28,1),
                       colours = c("red", "yellow", "green"),
                       trans = "sqrt",
                       name = "Predicted Rate of \nAgreement at the UN") +
  geom_sf() + ggtitle("Predicted UN Agreement with China")
dev.off()
