library(RPostgreSQL)
library(DMwR2)
library(DMwR)
library(tidyverse)
library (RODBC)
library (dlookr)
library (magrittr)
library(dplyr) 
library(SmartEDA)
library(PerformanceAnalytics)
library(DataExplorer)
library(Hmisc)
library(pastecs)
library(ISLR)

## connect to SQL Server ###

setwd("~/R")
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="AACT",host="ucd-capstone.ccsbkyvo5kcy.eu-west-1.rds.amazonaws.com", port=5432, user="students", password="ucdcapstone2022")
aact_sample <- dbGetQuery(con, "SELECT * FROM public.sample_studies")
aact_sample

## analysis of the data ###
describe(aact_sample)
stat.desc(aact_sample)
normality(aact_sample)
summary(aact_sample)

plot_str(aact_sample)
plot_missing(aact_sample)
plot_histogram(aact_sample)
plot_bar(aact_sample)

## Exploratory Analysis ##
create_report(aact_sample)
create_report(aact_sample,y="study_type")
create_report(aact_sample,y="overall_status")

# Overview of the data - Type = 1
type_1 = ExpData(data=aact_sample,type=1)
write.csv(type_1,"type_1_stats.csv")

# Structure of the data - Type = 2
type_2 = ExpData(data=aact_sample,type=2)
write.csv(type_2,"type_2_stats.csv")

# Metadata Information with additional statistics like mean, median and variance
type_3 = ExpData(data=aact_sample,type=2, fun = c("mean", "median", "var"))
write.csv(type_3,"type_3_stats.csv")

type_4= ExpNumStat(aact_sample,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)
write.csv(type_4,"type_4_stats.csv")

# Note: Variable excluded (if unique value of variable which is less than or eaual to 10 [nlim=10])
plot1 <- ExpNumViz(aact_sample,target=NULL,nlim=10,Page=c(2,2),sample=4)
plot1[[1]]

y=ExpCTable(aact_sample,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)
write.csv(y,"stats.csv")

plot2 <- ExpCatViz(aact_sample,target=NULL,col ="slateblue4",clim=10,margin=2,Page = c(2,2),sample=4)
plot2[[1]]

### Diagnose Report ###

diagnose(aact_sample)
aact_sample %>%
  diagnose_report(output_format = "html", output_file = "Diagn.html")

aact_sample %>%
  eda_report(output_format = "html",target=NULL, output_file = "EDA_studies.html")

glimpse(aact_sample)
summary(aact_sample)
cor(aact_sample)
chisq.test(aact_sample)

### Clean Data #####


sample = aact_sample


#NCT id
is.null(studies$nct_id)
str(studies$nct_id)

#Study Type 
table(sample$study_type)
table
sample <- sample[!(sample$study_type == "N/A"),]
table(sample$study_type)
sample <- droplevels(sample)
table(sample$study_type)


#Phase
is.null(sample$phase)
str(sample$phase)
table(sample$phase)
#convert to character
sample$phase <- as.character(sample$phase)
#assiging "Observational" to every "N/A" entry
sample$phase[sample$phase == "N/A"] <- "Not_Provided"
sample$phase[sample$phase == "NULL"] <- "Not_Interventional"
#convert back to levels
sample$phase <- as.factor(sample$phase)
table(sample$phase)


#why stopped 
str(sample$why_stopped)
#making new column
sample$stopped <- "stopped"
sample$stopped <- as.character(sample$stopped)
sample$stopped[sample$why_stopped == "NULL"] <- "not stopped/ not stated"
sample$stopped <- as.factor(sample$stopped)
str(sample$stopped)
table(sample$stopped)
#changing nulls of original column
sample$why_stopped <- as.character(sample$why_stopped)
sample$why_stopped[sample$why_stopped == "NULL"] <- "not stopped/ not stated"



#Gender
#checking levels
str(sample$gender)
#assiging "gender not specified" to every "ALL" entry
sample$gender <- as.character(sample$gender)
sample$gender[sample$gender == "All"] <- "no_criteria"
sample$gender[sample$gender == ""] <- "no_criteria"
#convert back to levels
sample$gender <- as.factor(sample$gender)
table(sample$gender)

#min age
#checking levels 
str(sample$minimum_age)
sample$minimum_age <- as.character(sample$minimum_age)
sample$minimum_age[sample$minimum_age == "N/A"] <- "no_criteria"
#convert back to levels
sample$minimum_age <- as.factor(sample$minimum_age)
levels(sample$minimum_age)
sample <- droplevels(sample)



#max age
#checking levels 
str(sample$maximum_age)
sample$maximum_age <- as.character(sample$maximum_age)
sample$maximum_age[sample$maximum_age == "N/A"] <- "no_criteria"
#convert back to levels
sample$maximum_age <- as.factor(sample$maximum_age)
levels(sample$maximum_age)


#overall status
str(sample$overall_status)
table(sample$overall_status)

######start-Month year
str(sample$start_month_year)
sample$start_month_year <- as.character(sample$start_month_year)
sample$start_month_year[sample$start_month_year == "NULL" ] <- "not_provided"
sample$start_month_year <- as.factor(sample$start_month_year)

##completion month year
str(sample$primary_completion_month_year)
sample$primary_completion_month_year <- as.character(sample$primary_completion_month_year)
sample$primary_completion_month_year[sample$primary_completion_month_year == "NULL" ] <- "not_provided"
sample$primary_completion_month_year <- as.factor(sample$primary_completion_month_year)

#enrollment
str(sample$enrollment)
sample$enrollment <- as.character(sample$enrollment)
sample$enrollment[sample$enrollment == "NULL" ] <- "not_provided"
sample$enrollment <- as.factor(sample$enrollment)


#has dmc 
str(sample$has_dmc)
sample$has_dmc <- as.character(sample$has_dmc)
sample$has_dmc[sample$has_dmc == "NULL" ] <- "not_provided"
sample$has_dmc <- as.factor(sample$has_dmc)

#official title
str(sample$official_title)
sample$official_title <- as.character(sample$official_title)
sample$official_title[sample$official_title == "NULL" ] <- "not_provided"
sample$official_title <- as.factor(sample$official_title)

#brief title
str(sample$brief_title)
sample$brief_title <- as.character(sample$brief_title)
sample$brief_title[sample$brief_title == "NULL" ] <- "not_provided"
sample$brief_title <- as.factor(sample$brief_title)

#enrollment type
str(sample$enrollment_type)
sample$enrollment_type <- as.character(sample$enrollment_type)
sample$enrollment_type[sample$enrollment_type == "NULL" ] <- "not_provided"
sample$enrollment_type <- as.factor(sample$enrollment_type)
table(sample$enrollment_type) 

#study first submitted date
str(sample$study_first_submitted_date)
sample$study_first_submitted_date <- as.character(sample$study_first_submitted_date)
sample$study_first_submitted_date[sample$study_first_submitted_date == "NULL" ] <- "not_provided"
sample$study_first_submitted_date <- as.factor(sample$study_first_submitted_date)

#results first posted date
str(sample$results_first_posted_date)
sample$results_first_posted_date <- as.character(sample$results_first_posted_date)
sample$results_first_posted_date[sample$results_first_posted_date == "NULL" ] <- "not_provided"
sample$results_first_posted_date <- as.factor(sample$results_first_posted_date)



#number of facicilities
str(sample$number_of_facilities)
sample$number_of_facilities <- as.character(sample$number_of_facilities)
sample$number_of_facilities[sample$number_of_facilities == "NULL" ] <- "not_provided"
sample$number_of_facilities <- as.factor(sample$number_of_facilities)


#number of arms
str(sample$number_of_arms)
sample$number_of_arms <- as.character(sample$number_of_arms)
sample$number_of_arms[sample$number_of_arms == "NULL" ] <- "not_provided"
sample$number_of_arms <- as.factor(sample$number_of_arms)

#number of groups
str(sample$number_of_groups)
sample$number_of_groups <- as.character(sample$number_of_groups)
sample$number_of_groups[sample$number_of_groups == "NULL" ] <- "not_provided"
sample$number_of_groups <- as.factor(sample$number_of_groups)

#countries
str(sample$country_name)
sample$country_name <- as.character(sample$country_name)
sample$country_name[sample$country_name == "NULL" ] <- "not_provided"
sample$country_name <- as.factor(sample$country_name)


#condition name 
str(sample$condition_name)
sample$condition_name <- as.character(sample$condition_name)
sample$condition_name[sample$condition_name == "NULL" ] <- "not_provided"
sample$condition_name <- as.factor(sample$condition_name)


#actual duration
str(sample$actual_duration)
sample$actual_duration <- as.character(sample$actual_duration)
sample$actual_duration[sample$actual_duration == "NULL" ] <- "not_provided"
sample$actual_duration <- as.factor(sample$actual_duration)

#were results reported
str(sample$were_results_reported)
table(sample$were_results_reported)

#has sigle facility 
str(sample$has_single_facility)
table(sample$has_single_facility)



#allocation
str(sample$allocation)
sample$allocation <- as.character(sample$allocation)
sample$allocation[sample$allocation == "NULL" ] <- "not_provided"
sample$allocation[sample$allocation == "N/A" ] <- "not_provided"
sample$allocation <- as.factor(sample$allocation)
table(sample$allocation)

#masking
str(sample$masking)
sample$masking <- as.character(sample$masking)
sample$masking[sample$masking == "NULL" ] <- "not_provided"
sample$masking <- as.factor(sample$masking)
table(sample$masking)

#interventional model 
str(sample$intervention_model)
sample$intervention_model <- as.character(sample$intervention_model)
sample$intervention_model[sample$intervention_model == "NULL" ] <- "none"
sample$intervention_model <- as.factor(sample$intervention_model)
table(sample$intervention_model)

#observationall model 
str(sample$observational_model)
sample$observational_model <- as.character(sample$observational_model)
sample$observational_model[sample$observational_model == "NULL" ] <- "none"
sample$observational_model <- as.factor(sample$observational_model)
table(sample$observational_model)


#primary purpose 
str(sample$primary_purpose)
sample$primary_purpose <- as.character(sample$primary_purpose)
sample$primary_purpose[sample$primary_purpose == "NULL" ] <- "not_provided"
sample$primary_purpose <- as.factor(sample$primary_purpose)
table(sample$primary_purpose)


#healthy volunteers
table(sample$healthy_volunteers)
sample$healthy_volunteers <- as.character(sample$healthy_volunteers)
sample$healthy_volunteers[sample$healthy_volunteers == "" ] <- "not_provided"
sample$healthy_volunteers <- as.factor(sample$healthy_volunteers)

#population
table(sample$population)
str(sample$population)
sample$population <- as.character(sample$population)
sample$population[sample$population == "" ] <- "not_provided"
sample$population <- as.factor(sample$population)

#sampling method
table(sample$sampling_method)
str(sample$sampling_method)
sample$sampling_method <- as.character(sample$sampling_method)
sample$sampling_method[sample$sampling_method == "" ] <- "not_provided"
sample$sampling_method <- as.factor(sample$sampling_method)

#gender based 
table(sample$gender_based)
sample$gender_based <- as.character(sample$gender_based)
sample$gender_based[sample$gender_based == "NULL" ] <- "FALSE"
sample$gender_based <- as.factor(sample$gender_based)


#intervention type 
table(sample$intervention_type)
sample$intervention_type <- as.character(sample$intervention_type)
sample$intervention_type[sample$intervention_type == "NULL" ] <- "not_provided"
sample$intervention_type <- as.factor(sample$intervention_type)

#sponsor name 
str(sample$lead_Sponsor)
sample$lead_Sponsor <- as.character(sample$lead_Sponsor)
sample$lead_Sponsor[sample$lead_Sponsor == "NULL" ] <- "not_provided"
sample$lead_Sponsor <- as.factor(sample$lead_Sponsor)

#agency class
str(sample$Agency_Class_Lead)
table(sample$Agency_Class_Lead)
sample$Agency_Class_Lead <- as.character(sample$Agency_Class_Lead)
sample$Agency_Class_Lead[sample$Agency_Class_Lead == "NULL" ] <- "not_provided"
sample$Agency_Class_Lead <- as.factor(sample$Agency_Class_Lead)

#lead or collaborator
str(sample$lead_or_collaborator)
table(sample$lead_or_collaborator)

#subjects affected
str(sample$subjects_affected)
sample$subjects_affected <- as.character(sample$subjects_affected)
sample$subjects_affected[sample$subjects_affected == "NULL" ] <- "not_provided"
sample$subjects_affected <- as.factor(sample$subjects_affected)

#subjects at risk
str(sample$subjects_at_risk)
sample$subjects_at_risk <- as.character(sample$subjects_at_risk)
sample$subjects_at_risk[sample$subjects_at_risk == "NULL" ] <- "not_provided"
sample$subjects_at_risk <- as.factor(sample$subjects_at_risk)

#responsible party type
str(sample$responsible_party_type)
table(sample$responsible_party_type)
sample$responsible_party_type <- as.character(sample$responsible_party_type)
sample$responsible_party_type[sample$responsible_party_type == "NULL" ] <- "not_provided"
sample$responsible_party_type <- as.factor(sample$responsible_party_type)

#key words name
str(sample$keywords_name)
sample$keywords_name <- as.character(sample$keywords_name)
sample$keywords_name[sample$keywords_name == "NULL" ] <- "not_provided"
sample$keywords_name <- as.factor(sample$keywords_name)


#seperating studies

write.csv(sample,"sample_studies_cleaned.csv")

cleaned = read.csv("sample_studies_cleaned.csv")

### Numerical values bins were transformed in Excel #####

