library(tidyverse)

setwd("C:/Users/jeffr/Downloads/POLI178/Final_Project/Natural_Resources_Rents_Percent_GDP")

forest <- read.csv("Forest_Rents.csv", skip = 4)
minerals <- read.csv("Mineral_Rents.csv", skip = 4)
natural_gas <- read.csv("Natural_Gas_Rents.csv", skip = 4)
coal <- read.csv("Coal_Rents.csv", skip = 4)
oil <- read.csv("Oil_Rents.csv", skip = 4)
total <- read.csv("Total_Natural_Resources_Rents.csv", skip = 4)

df <- bind_rows(coal, forest, minerals, natural_gas, oil, total)

write.csv(df, "All_Rents_Data.csv")
