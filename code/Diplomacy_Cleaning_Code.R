#Set Wd
setwd("C:/Users/Andrew/Desktop/POLI178")

#load in datasets
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)
China_perception_data = read_excel("Perceptions.xlsx")
China_public_diplomacy_data = read_excel("ChinesePublicDiplomacy.xlsx")
China_confucius_institutes_data = read_excel("Confucius-Institutes.xlsx")
China_finanical_diplomacy_data = read_excel("ChineseFinancialPublicDiplomacyProjectDetails.xlsx")

#Check data for completeness
View(China_perception_data)
View(China_public_diplomacy_data)
View(China_confucius_institutes_data)
View(China_finanical_diplomacy_data)

#Perceptions and Confucius datasets are complete and square.
#Need to change all string numbers into numeric values

#Str to Numeric
China_perception_data = China_perception_data %>% 
  mutate_at(c('perceptions_of_influence',	'positivity' , 
              'perceptions_of_helpfulness'), as.numeric)
China_confucius_institutes_data = China_confucius_institutes_data %>% 
  mutate_at(c('year_opened', 'year_closed'), as.numeric)

#Visualize perception and confucius data

ggplot(China_confucius_institutes_data, aes(x=region)) +
  labs(title = "Number of Confucius Institutes by Region", x = "Region")+
  geom_bar() +
  theme_light()

ggplot(China_confucius_institutes_data, aes(x=status)) +
  labs(title = "Number of Active and Inactive Confucius Institutes", x = "Status")+
  geom_bar() +
  theme_light()

#Data is clean and ready for analysis. No further cleaning required.

#Save datasets
write_csv(China_perception_data, "C:/Users/Andrew/Desktop/POLI178/china_perception_data.csv")
write_csv(China_confucius_institutes_data, "C:/Users/Andrew/Desktop/POLI178/china_confucius_institutes_data.csv")

#Clean public diplomacy

#Check for NA's
anyNA(China_public_diplomacy_data)

#Returns False. Dataset however has many NA's meaning they are strings.
#Possibly true for numeric values as well

#Replace string value with proper NA value
China_public_diplomacy_data[China_public_diplomacy_data == "N/A"] <- NA

#Replace string value with proper numeric value
China_public_diplomacy_data = China_public_diplomacy_data %>% 
  mutate_at(c('year', 'confucius_institutes', 'confucius_classrooms',
              'sister_cities_established', 'outbound_political_visits',
              'inbound_political_visits' , 'broader_cadre_visits' ,
              'ccp_visits' , 'ambassador_op_eds' ,	'journalist_visits' ,	
              'content_sharing_partnerships' , 'outbound_chinese_students' ,	
              'inbound_students_to_china' ,	'expert_deployments' ,	
              'joint_resource_developments' ,	'scholarships' ,	'training_programs'), as.numeric)


#Count NA's
colSums(is.na(China_public_diplomacy_data))

#Count non NA's
colSums(!is.na(China_public_diplomacy_data))

#Count 0's
colSums(China_public_diplomacy_data==0)

#Determine how much of the dataset is NA
colMeans(is.na(China_public_diplomacy_data))*100

#Majority of the dataset has more then 50% missing entries for the variables.
#Outlier is content_sharing_partnerships and confucius_institutes

#Check content_sharing_partnerships and confucius_institutes
hist(China_public_diplomacy_data$content_sharing_partnerships)
hist(China_public_diplomacy_data$confucius_institutes)

#Public Diplomacy data appears to be unusable due to the amount of 0 and NA data.
#Seems only inbound and outbound students variable is consistent with some value.
#The rest however doesn't seem that way.
#Will leave alone for now, consult with team on what to do with it.

#Clean financial data

# String to numeric
China_finanical_diplomacy_data = China_finanical_diplomacy_data %>% 
  mutate_at(c('commitment_year', 'implementation_start_year' , 'completion_year',
              'sector_code', 'amount_original_currency', 'amount_constant_usd2017',	
              'amount_nominal', 'maturity', 'interest_rate',	'grace_period', 'management_fee',
              'commitment_fee',	'grant_element', 'source_quality_score',	'data_completeness_score',
              'project_implementation_score',	'loan_detail_score'), as.numeric)
#Drop columns
China_finanical_diplomacy_data = China_finanical_diplomacy_data %>% select(-project_id, -recommended_for_research, -umbrella_project,
                                          -title, -description, -staff_comments,
                                          -receiving_agencies_origin, -implementing_agencies_origin,
                                          -accountable_agencies_origin, -planned_implementation_start_date,	
                                          -planned_completion_date, -official_source_count,	-unofficial_source_count,
                                          -source_urls,	-source_titles,	-source_publishers,	-source_type,	-location_details,
                                          -geojson_url_viz,	-geojson_url_dl, -contact_name,	-contact_position,	-oda_eligible_recipient)
#Save financial diplomacy
write_csv(China_finanical_diplomacy_data, "C:/Users/Andrew/Desktop/POLI178/china_financial_diplomacy_data.csv")

#Visualize financial diplomacy
ggplot(China_finanical_diplomacy_data, aes(x=recipient)) +
  geom_bar()

hist(China_finanical_diplomacy_data$project_implementation_score)

hist(China_finanical_diplomacy_data$data_completeness_score)

hist(China_finanical_diplomacy_data$source_quality_score)

hist(China_finanical_diplomacy_data$loan_detail_score)

ggplot(China_finanical_diplomacy_data, aes(x=flow_type)) +
  labs(title = "Frequency of different types of financial support", x = "Types")+
  geom_bar() +
  theme_light()

ggplot(China_finanical_diplomacy_data, aes(x=intent)) +
  labs(title = "Frequency of Intent of financial investment", x = "Intent")+
  geom_bar() +
  theme_light()

ggplot(China_finanical_diplomacy_data, aes(x=recipient_region)) +
  labs(title = "Frequency of financial diplomacy by Region", x = "Region")+
  geom_bar() +
  theme_light()


#Seems there are a few countires that have gotten the most from China.

#Most frequent country
names(which.max(table(China_finanical_diplomacy_data$recipient)))

#Angola is the most frequent

#Most frequent sector
names(which.max(table(China_finanical_diplomacy_data$sector_name)))

#Education is the most frequent sector

#Top 10 most frequent countries
sort(table(China_finanical_diplomacy_data$recipient), decreasing = TRUE)[1:10]

#The top 10 countries are Angola, Cambodia, Myanmar, Sudan, Pakistan, Indonesia, DR Congo, Laos, Ethiopia, and Tanzania.

#Top 10 most frequent sectors
sort(table(China_finanical_diplomacy_data$sector_name), decreasing = TRUE)[1:10]




