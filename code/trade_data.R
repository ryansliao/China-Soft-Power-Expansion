library(tidyverse)
library(dbplyr)
library(plyr)
library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)
library(kableExtra)
library(countrycode)
library(viridis)
library(ggthemes)
library(broom)

setwd("~/College Assignments/POLI 178/Trade Data")

### Arms Imports
china_arms_import <- read_csv("china_arms_imports_tiv.csv")
years_elim <- 1950:1990
years_elim <- as.character(years_elim)
china_arms_import[is.na(china_arms_import)] <- 0
china_arms_import <- select(china_arms_import, -years_elim)
china_arms_import <- select(china_arms_import, -c('Total'))
china_arms_import$Total_imports <- rowSums(china_arms_import[2:32])
china_arms_import <- head(china_arms_import, -1)
names(china_arms_import)[names(china_arms_import) == "...1"] <- "name_long"

world <- ne_countries(scale = "medium", returnclass = "sf")
china_arms_import_geo <- merge(world, china_arms_import[c('name_long', 'Total_imports')], by="name_long", all=TRUE)
china_arms_import_geo[is.na(china_arms_import_geo)] <- 0

ggplot(data = china_arms_import_geo) +
  geom_sf(aes(fill = Total_imports)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "Chinese Arms Imports", fill = "Total Imports")

### Arms Imports Regression
china_arms_import <- read_csv("china_arms_imports_tiv.csv")
years_elim <- 1950:1990
years_elim <- as.character(years_elim)
china_arms_import[is.na(china_arms_import)] <- 0
china_arms_import <- select(china_arms_import, -years_elim)
china_arms_import <- select(china_arms_import, -c('Total'))
china_arms_import <- china_arms_import %>% 
  pivot_longer(
    !`...1`, 
    names_to = "year", 
    values_to = "imports"
  )
names(china_arms_import)[names(china_arms_import) == "...1"] <- "name_long"
fitted_models <- china_arms_import %>% group_by(name_long) %>% do(model = lm(imports~year, china_arms_import))
models <- dlply(china_arms_import, "name_long", function(df) 
  lm(imports ~ year, data = df))
sums <- ldply(models, coef)
sums
arms_regs <- cbind(sums[1], sums[2])
names(arms_regs)[names(arms_regs) == "(Intercept)"] <- "coef"
arms_regs$coef <- log10(arms_regs$coef + 1)

world <- ne_countries(scale = "medium", returnclass = "sf")
china_arms_reg_geo <- merge(world, arms_regs[c('name_long', 'coef')], by="name_long", all=TRUE)
china_arms_reg_geo[is.na(china_arms_reg_geo)] <- 0

ggplot(data = china_arms_reg_geo) + 
  geom_sf(aes(fill = coef)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Chinese Arms Import Growth", fill = "Coefficients")

### Arms Exports
china_arms_export <- read_csv("china_arms_exports_tiv.csv")
years_elim <- 1950:1990
years_elim <- as.character(years_elim)
china_arms_export[is.na(china_arms_export)] <- 0
china_arms_export <- select(china_arms_export, -years_elim)
china_arms_export <- select(china_arms_export, -c('Total'))
china_arms_export$Total_Exports <- rowSums(china_arms_export[2:32])
china_arms_export <- head(china_arms_export, -1)
names(china_arms_export)[names(china_arms_export) == "...1"] <- "name_long"

world <- ne_countries(scale = "medium", returnclass = "sf")
china_arms_export_geo <- merge(world, china_arms_export[c('name_long', 'Total_Exports')], by="name_long", all=TRUE)
china_arms_export_geo[is.na(china_arms_export_geo)] <- 0

ggplot(data = china_arms_export_geo) +
  geom_sf(aes(fill = Total_Exports)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title = "Chinese Arms Exports", fill = "Total Exports")

### Arms Exports Regression
china_arms_export <- read_csv("china_arms_exports_tiv.csv")
years_elim <- 1950:1990
years_elim <- as.character(years_elim)
china_arms_export[is.na(china_arms_export)] <- 0
china_arms_export <- select(china_arms_export, -years_elim)
china_arms_export <- select(china_arms_export, -c('Total'))
china_arms_export <- china_arms_export %>% 
  pivot_longer(
    !`...1`, 
    names_to = "year", 
    values_to = "exports"
  )
names(china_arms_export)[names(china_arms_export) == "...1"] <- "name_long"
fitted_models <- china_arms_export %>% group_by(name_long) %>% do(model = lm(exports~year, china_arms_export))
models <- dlply(china_arms_export, "name_long", function(df) 
  lm(exports ~ year, data = df))
sums <- ldply(models, coef)
sums
arms_regs <- cbind(sums[1], sums[2])
names(arms_regs)[names(arms_regs) == "(Intercept)"] <- "coef"
arms_regs$coef <- log10(arms_regs$coef + 1)

world <- ne_countries(scale = "medium", returnclass = "sf")
china_arms_reg_geo <- merge(world, arms_regs[c('name_long', 'coef')], by="name_long", all=TRUE)
china_arms_reg_geo[is.na(china_arms_reg_geo)] <- 0

ggplot(data = china_arms_reg_geo) + 
  geom_sf(aes(fill = coef)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Chinese Arms Emport Growth", fill = "Coefficients")

### Imports
china_import <- read_excel("china_import.xlsx", sheet = 2)
china_import <- select(china_import, -c("Reporter Name", "Partner Name", "Trade Flow", "Indicator"))
china_import <- china_import %>% 
  pivot_longer(
    !`Product Group`, 
    names_to = "year", 
    values_to = "imports"
  )

ggplot(china_import, aes(x = year, y = imports))+
  geom_line(aes(group=1)) +
  geom_point() +
  labs(title = "Chinese Import Total", x = "Year", y = "Imports ($)")

### Africa Imports
china_import_africa <- read_excel("china_import_subsaharan_africa.xlsx", sheet = 2)
china_import_africa <- select(china_import_africa, -c("Reporter Name", "Partner Name", "Trade Flow", "Indicator"))
china_import_africa <- china_import_africa %>% 
  pivot_longer(
    !`Product Group`, 
    names_to = "year", 
    values_to = "imports",
  )
names(china_import_africa)[names(china_import_africa) == "Product Group"] <- "product_group"

ggplot(china_import_africa, aes(x = year, y = imports, color = product_group, group = product_group)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Chinese Imports to Sub-Saharan Africa", x = "Year", y = "Imports ($)", color = "Product Group")

### Exports
china_export <- read_excel("china_export.xlsx", sheet = 2)
china_export <- select(china_export, -c("Reporter Name", "Partner Name", "Trade Flow", "Indicator"))
china_export <- china_export %>% 
  pivot_longer(
    !`Product Group`, 
    names_to = "year", 
    values_to = "exports"
  )

ggplot(china_export, aes(x = year, y = exports))+
  geom_line(aes(group=1)) +
  geom_point() +
  labs(title = "Chinese Export Total", x = "Year", y = "Exports ($)")

### Africa Exports
china_export_africa <- read_excel("china_export_subsaharan_africa.xlsx", sheet = 2)
china_export_africa <- select(china_export_africa, -c("Reporter Name", "Partner Name", "Trade Flow", "Indicator"))
china_export_africa <- china_export_africa %>% 
  pivot_longer(
    !`Product Group`, 
    names_to = "year", 
    values_to = "exports",
  ) 
names(china_export_africa)[names(china_export_africa) == "Product Group"] <- "product_group"

ggplot(china_export_africa, aes(x = year, y = exports, color = product_group, group = product_group)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Chinese Exports to Sub-Saharan Africa", x = "Year", y = "Exports ($)", color = "Product Group")

write.csv(china_export, "~/College Assignments/POLI 178/Trade Data/china_export_clean.csv")
write.csv(china_export_africa, "~/College Assignments/POLI 178/Trade Data/china_export_africa_clean.csv")
write.csv(china_arms_export, "~/College Assignments/POLI 178/Trade Data/china_arms_export_clean.csv")
write.csv(china_import, "~/College Assignments/POLI 178/Trade Data/china_import_clean.csv")
write.csv(china_import_africa, "~/College Assignments/POLI 178/Trade Data/china_import_africa_clean.csv")
write.csv(china_arms_import, "~/College Assignments/POLI 178/Trade Data/china_arms_import_clean.csv")
