#Packages
library(readxl)
library(ggplot2)
library(dplyr)
library(abind)
library(tidyr)
library(mice)
library(estimatr)
library(howManyImputations)
library(miceadds)

#Opening spreadsheets
qualtrics_data <- read_excel("/Users/gmcgannon/Library/CloudStorage/OneDrive-LondonSchoolofEconomics/T3 - MY499 - dissertation/Survey data/Dissertation_survey_8_April_2024.xls")
prolific_data <- read_excel("/Users/gmcgannon/Library/CloudStorage/OneDrive-LondonSchoolofEconomics/T3 - MY499 - dissertation/Survey data/Demographic_data_8_April_2024.xls")

#Renaming participant id column in qualtrics for merging
names(qualtrics_data)[names(qualtrics_data) == "Q31"] <- "Participant id"

#Merging qualtrics and prolific 
MY499_data <- merge(prolific_data, qualtrics_data, by = "Participant id", all = TRUE)

###Cleaning data 
#Removing invalid responses
MY499_data <- subset(MY499_data, Status == "APPROVED")
MY499_data <- subset(MY499_data, Progress == "100")
MY499_data <- subset(MY499_data, Refusal_1 != "incorrect response" | is.na(Refusal_1))
MY499_data <- subset(MY499_data, Permission_1 != "incorrect response" | is.na(Permission_1))
MY499_data <- subset(MY499_data, Q22 != "Younger than 11")
MY499_data <- subset(MY499_data, Q22 != "Older than 16")

#Updating column names
names(MY499_data)[names(MY499_data) == "Participant id"] <- "participant_id"
names(MY499_data)[names(MY499_data) == "Year of birth of youngest child"] <- "yob_youngest"
names(MY499_data)[names(MY499_data) == "Household income (gbp)"] <- "household_income"
names(MY499_data)[names(MY499_data) == "Current uk area of residence"] <- "region"
names(MY499_data)[names(MY499_data) == "Socioeconomic status"] <- "ses_self_report"
names(MY499_data)[names(MY499_data) == "Number of children"] <- "no_children"
names(MY499_data)[names(MY499_data) == "Ethnicity simplified"] <- "ethnicity"
names(MY499_data)[names(MY499_data) == "Country of birth"] <- "country_birth"
names(MY499_data)[names(MY499_data) == "Employment status"] <- "employment"
names(MY499_data)[names(MY499_data) == "Duration (in seconds)"] <- "duration"
names(MY499_data)[names(MY499_data) == "Q1"] <- "NSSEC_1"
names(MY499_data)[names(MY499_data) == "Q13"] <- "NSSEC_2"
names(MY499_data)[names(MY499_data) == "Q14"] <- "NSSEC_3"
names(MY499_data)[names(MY499_data) == "Q15"] <- "NSSEC_4"
names(MY499_data)[names(MY499_data) == "Q16"] <- "NSSEC_5"
names(MY499_data)[names(MY499_data) == "Q22"] <- "child_age"
names(MY499_data)[names(MY499_data) == "Q27"] <- "fsm"
names(MY499_data)[names(MY499_data) == "Q35"] <- "no_activities"
names(MY499_data)[names(MY499_data) == "Q36"] <- "confidence"
names(MY499_data)[names(MY499_data) == "Q3"] <- "control_response"
names(MY499_data)[names(MY499_data) == "Q37_Page Submit"] <- "control_time"
names(MY499_data)[names(MY499_data) == "Q8"] <- "enrichment_response"
names(MY499_data)[names(MY499_data) == "Q38_Page Submit"] <- "enrichment_time"
names(MY499_data)[names(MY499_data) == "Q10"] <- "simple_response"
names(MY499_data)[names(MY499_data) == "Q39_Page Submit"] <- "simple_time"
names(MY499_data)[names(MY499_data) == "Q4"] <- "understand_1"
names(MY499_data)[names(MY499_data) == "Q5"] <- "understand_2"
names(MY499_data)[names(MY499_data) == "Q6"] <- "understand_3"
names(MY499_data)[names(MY499_data) == "Q25"] <- "understand_4"

#Creating assignment condition indicator
MY499_data$condition <- ifelse(!is.na(MY499_data$control_response), "control",
                               ifelse(!is.na(MY499_data$enrichment_response), "enrichment",
                                      ifelse(!is.na(MY499_data$simple_response), "simple", NA)))

#Creating assignment dummies
MY499_data$condition <- ifelse(!is.na(MY499_data$control_response), "control",
                               ifelse(!is.na(MY499_data$enrichment_response), "enrichment",
                                      ifelse(!is.na(MY499_data$simple_response), "simple", NA)))
MY499_data$control_dummy <- ifelse(!is.na(MY499_data$control_response), 1, 0)
MY499_data$enrichment_dummy <- ifelse(!is.na(MY499_data$enrichment_response), 1, 0)
MY499_data$simple_dummy <- ifelse(!is.na(MY499_data$simple_response), 1, 0)
MY499_data$enrichment_condition <- ifelse(MY499_data$enrichment_dummy == 1, 1,
                                          ifelse(MY499_data$control_dummy == 1, 0,
                                                 ifelse(MY499_data$simple_dummy == 0, NA, NA)))
MY499_data$simple_condition <- ifelse(MY499_data$simple_dummy == 1, 1,
                                      ifelse(MY499_data$control_dummy == 1, 0,
                                             ifelse(MY499_data$enrichment_dummy == 0, NA, NA)))

#Creating single consent indicator
MY499_data$consent <- ifelse(
  (!is.na(MY499_data$control_response) & MY499_data$control_response == "Yes") |
  (!is.na(MY499_data$enrichment_response) & MY499_data$enrichment_response == "Yes") |
    (!is.na(MY499_data$simple_response) & MY499_data$simple_response == "Yes"),
  1,
  0)

#Class dummy
MY499_data <- MY499_data %>%
  mutate(class = ifelse(NSSEC_5 %in% c(
    "Senior manager or administrators (usually responsible for planning, organising and co-ordinating work, and for finance) such as: finance manager - chief executive",
    "Clerical and intermediate occupations such as: secretary - personal assistant - clerical worker - office clerk - call centre agent - nursing auxiliary - nursery nurse",
    "Middle or junior managers such as: office manager - retail manager - bank manager - restaurant manager - warehouse manager - publican",
    "Modern professional occupations such as: teacher - nurse - physiotherapist - social worker - welfare officer - artist - musician - police officer (sergeant or above) - software designer",
    "Traditional professional occupations such as: accountant - solicitor - medical practitioner - scientist - civil/mechanical engineer"
  ), 1, ifelse(NSSEC_5 %in% c(
    "Routine manual and service occupations such as: HGV driver - van driver - cleaner - porter - packer - sewing machinist - messenger - labourer - waiter/waitress - bar staff",
    "Technical and craft occupations such as: motor mechanic - fitter - inspector - plumber - printer - tool maker - electrician - gardener - train driver",
    "Semi-routine manual and service occupations such as: postal worker - machine operative - security guard - caretaker - farm worker - caring assistant - receptionist - sales assistant"
  ) & (NSSEC_1 %in% c("Self-employed with employees", "Self-employed without employees")), 1, 0)))

#Creating score for knowledge questions
MY499_data$understand_1_correct <- ifelse(MY499_data$understand_1 == "Do a survey this school year,Do a survey next school year", 1, 0)
MY499_data$understand_2_correct <- ifelse(MY499_data$understand_2 == "No", 1, 0)
MY499_data$understand_3_correct <- ifelse(MY499_data$understand_3 == "Yes, up until a certain date", 1, 0)
MY499_data$understand_4_correct <- ifelse(MY499_data$understand_4 == "To write a report on NumeracyPlus,To write academic papers,For future research", 1, 0)

understand_columns <- c("understand_1_correct", "understand_2_correct", "understand_3_correct", "understand_4_correct")
understand_notNA <- rowSums(!is.na(MY499_data[understand_columns]))
understand_correct <- rowSums(MY499_data[understand_columns], na.rm = TRUE)
MY499_data$understand_correct <- understand_correct
MY499_data$understand_score <- understand_correct / understand_notNA
MY499_data <- MY499_data %>%
  mutate(understand_score = round(understand_score, digits = 2))

#Time spent reading PIS
MY499_data$pis_time <- coalesce(MY499_data$control_time, MY499_data$enrichment_time, MY499_data$simple_time)
MY499_data <- MY499_data %>%
  mutate(pis_minutes = round(pis_time / 60, digits = 1))

#Rounding duration to minutes
MY499_data <- MY499_data %>%
  mutate(duration_minutes = round(duration / 60, digits = 1))

#Sex dummy
MY499_data <- MY499_data %>%
  mutate(sex_dummy = ifelse(Sex == "Male", 0, 1))

#FSM dummy
MY499_data <- MY499_data %>%
  mutate(fsm_dummy = ifelse(fsm == "No", 1, 0))

#income dummy
MY499_data <- MY499_data %>%
  mutate(median_income = ifelse(household_income %in% c("¬£30,000 - ¬£39,999", 
                                                        "¬£40,000 - ¬£49,999",
                                                        "¬£50,000 - ¬£59,999",
                                                        "¬£60,000 - ¬£69,999",
                                                        "¬£70,000 - ¬£79,999",
                                                        "¬£80,000 - ¬£89,999",
                                                        "¬£90,000 - ¬£99,999",
                                                        "¬£100,000 - ¬£149,999",
                                                        "More than ¬£150,000"),
                                1, 0))

#Immigration dummy
MY499_data <- MY499_data %>%
  mutate(immigration_dummy = ifelse(Immigration == "Yes, I was born in the country I am now living in", 0, 1))

#Language dummy
MY499_data <- MY499_data %>%
  mutate(language_dummy = ifelse(Language == "English", 0, 1))

#Updating variable types
MY499_data$Age <- as.numeric(MY499_data$Age)
MY499_data$ses_self_report <- as.numeric(MY499_data$ses_self_report)
MY499_data$no_children <- as.numeric(MY499_data$no_children)
MY499_data$child_age <- as.numeric(MY499_data$child_age)
MY499_data$no_activities <- as.numeric(MY499_data$no_activities)

factor_confidence <- factor(MY499_data$confidence, 
                            ordered = TRUE, 
                            levels = c("Not confident at all", 
                                       "Slightly confident", 
                                       "Somewhat confident", 
                                       "Quite confident", 
                                       "Extremely confident"))
confidence_dummy <- as.integer(factor_confidence)
MY499_data$confidence_dummy <- confidence_dummy

factor_income <- factor(MY499_data$household_income, 
                        ordered = TRUE, 
                        levels = c("Less than ¬£10,000", 
                                   "¬£10,000 - ¬£15,999", 
                                   "¬£16,000 - ¬£19,999", 
                                   "¬£20,000 - ¬£29,999", 
                                   "¬£30,000 - ¬£39,999", 
                                   "¬£40,000 - ¬£49,999", 
                                   "¬£50,000 - ¬£59,999", 
                                   "¬£60,000 - ¬£69,999", 
                                   "¬£70,000 - ¬£79,999", 
                                   "¬£80,000 - ¬£89,999", 
                                   "¬£90,000 - ¬£99,999", 
                                   "¬£100,000 - ¬£149,999", 
                                   "More than ¬£150,000"),
                        labels = c("<10,000",
                                   "10,000-15,999",
                                   "16,000-19,999",
                                   "20,000-29,999",
                                   "30,000-39,999",
                                   "40,000-49,999",
                                   "50,000-59,999",
                                   "60,000-69,999",
                                   "70,000-79,999",
                                   "80,000-89,999",
                                   "90,000-99,999",
                                   "100,000-149,999",
                                   ">150,000"))
MY499_data$income_factor <- factor_income

factor_condition <- factor(MY499_data$condition, 
                    ordered = FALSE, 
                    levels = c("control", 
                               "enrichment", 
                               "simple"),
                    labels = c("control",
                               "enrichment",
                               "simple"))
MY499_data$condition <- factor_condition

factor_ethnicity <- factor(MY499_data$ethnicity, 
                        ordered = FALSE, 
                        levels = c("White",
                                   "Black",
                                   "Asian",
                                   "Mixed",
                                   "Other"),
                        labels = c("White",
                                   "Black",
                                   "Asian",
                                   "Mixed",
                                   "Other"))
MY499_data$ethnicity_factor <- factor_ethnicity

factor_employment <- factor(MY499_data$employment, 
                           ordered = FALSE, 
                           levels = c("Full-Time",
                                      "Part-Time",
                                      "Not in paid work (e.g. homemaker', 'retired or disabled)",
                                      "DATA_EXPIRED",
                                      "Unemployed (and job seeking)",
                                      "Other",
                                      "Due to start a new job within the next month"),
                           labels = c("Full-time",
                                      "Part-time",
                                      "Not working",
                                      "Missing",
                                      "Not working",
                                      "Other",
                                      "Not working"))
MY499_data$employment_factor <- factor_employment

employment_complete <- factor(MY499_data$employment, 
                            ordered = FALSE, 
                            levels = c("Full-Time",
                                       "Part-Time",
                                       "Not in paid work (e.g. homemaker', 'retired or disabled)",
                                       "DATA_EXPIRED",
                                       "Unemployed (and job seeking)",
                                       "Other",
                                       "Due to start a new job within the next month"),
                            labels = c("Full-time",
                                       "Part-time",
                                       "Not working",
                                       "NA",
                                       "Not working",
                                       "Other",
                                       "Not working"))
MY499_data$employment_complete <- employment_complete

factor_activities <- factor(MY499_data$no_activities, 
                            ordered = TRUE, 
                            levels = c("0",
                                       "1",
                                       "2",
                                       "3",
                                       "4",
                                       "5",
                                       "6",
                                       "7",
                                       "8",
                                       "9",
                                       "10"),
                            labels = c("0",
                                       "1-3",
                                       "1-3",
                                       "1-3",
                                       "4-6",
                                       "4-6",
                                       "4-6",
                                       "7-9",
                                       "7-9",
                                       "7-9",
                                       "10 and over"))
MY499_data$activities_factor <- factor_activities

#Simple ethnicity dummy
MY499_data$ethnicity_simple <- ifelse(MY499_data$ethnicity == "White", 0, 1)
str(MY499_data)

#NA checks
fsm_na <- sum(is.na(MY499_data$fsm))
fsm_na
confidence_na <- sum(is.na(MY499_data$confidence))
confidence_na
income_na <- sum(MY499_data$household_income == "DATA_EXPIRED")
income_na
employment_na <- sum(MY499_data$employment == "DATA_EXPIRED")
employment_na
Age_na <- sum(MY499_data$Age == "DATA_EXPIRED")
Age_na
Sex_na <- sum(MY499_data$Sex == "DATA_EXPIRED")
Sex_na
ethnicity_na <- sum(MY499_data$ethnicity == "DATA_EXPIRED")
ethnicity_na
ses_na <- sum(MY499_data$ses_self_report == "DATA_EXPIRED")
ses_na
language_na <- sum(MY499_data$language == "DATA_EXPIRED")
language_na
immigrant_na <- sum(MY499_data$Immigration == "DATA_EXPIRED")
immigrant_na
nochildren_na <- sum(MY499_data$no_children == "DATA_EXPIRED")
nochildren_na
childage_na <- sum(MY499_data$no_child_age == "DATA_EXPIRED")
childage_na
activities_na <- sum(is.na(MY499_data$no_activities))
activities_na
understanding_na <- sum(rowSums(is.na(MY499_data[c("understand_1_correct", "understand_2_correct", "understand_3_correct", "understand_4_correct")])) > 0)
understanding_na
understanding1_na <- sum(is.na(MY499_data$understand_1_correct))
understanding1_na
understanding2_na <- sum(is.na(MY499_data$understand_2_correct))
understanding2_na
understanding3_na <- sum(is.na(MY499_data$understand_3_correct))
understanding3_na
understanding4_na <- sum(is.na(MY499_data$understand_4_correct))
understanding4_na

#Dummies for open text responses
MY499_data$personal_data <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "personal data") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "personal data") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "personal data"),
  1,
  0)
MY499_data$no_benefit <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "no benefit") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "no benefit") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "no benefit"),
  1,
  0)
MY499_data$gender_refuse <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "gender") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "gender") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "gender"),
  1,
  0)
MY499_data$pressure <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "pressure") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "pressure") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "pressure"),
  1,
  0)
MY499_data$mistrust_research <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "mistrust in research") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "mistrust in research") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "mistrust in research"),
  1,
  0)
MY499_data$child_preference_R <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "child preference") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "child preference") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "child preference"),
  1,
  0)
MY499_data$misunderstanding <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "misunderstanding") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "misunderstanding") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "misunderstanding"),
  1,
  0)
MY499_data$effort <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "effort") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "effort") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "effort"),
  1,
  0)
MY499_data$school_involvement <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "school involvement") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "school involvement") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "school involvement"),
  1,
  0)
MY499_data$SEN <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "SEN") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "SEN") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "SEN"),
  1,
  0)
MY499_data$lack_information <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "lack of information") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "lack of information") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "lack of information"),
  1,
  0)
MY499_data$other_R <- ifelse(
  (!is.na(MY499_data$Refusal_1) & MY499_data$Refusal_1 == "other") |
    (!is.na(MY499_data$Refusal_2) & MY499_data$Refusal_2 == "other") |
    (!is.na(MY499_data$Refusal_3) & MY499_data$Refusal_3 == "other"),
  1,
  0)
MY499_data$no_harms <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "no harms") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "no harms") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "no harms") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "no harms"),
  1,
  0)
MY499_data$value_research <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "value of research") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "value of research") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "value of research") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "value of research"),
  1,
  0)
MY499_data$personal_benefit <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "personal benefit") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "personal benefit") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "personal benefit") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "personal benefit"),
  1,
  0)
MY499_data$data_compliance <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "data compliance") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "data compliance") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "data compliance") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "data compliance"),
  1,
  0)
MY499_data$not_intrusive <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "not intrusive") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "not intrusive") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "not intrusive") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "not intrusive"),
  1,
  0)
MY499_data$trust_school <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "trust in school") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "trust in school") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "trust in school") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "trust in school"),
  1,
  0)
MY499_data$trust_LSE <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "trust in LSE") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "trust in LSE") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "trust in LSE") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "trust in LSE"),
  1,
  0)
MY499_data$full_information <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "full information") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "full information") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "full information") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "full information"),
  1,
  0)
MY499_data$child_preference_C <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "child preference") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "child preference") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "child preference") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "child preference"),
  1,
  0)
MY499_data$careers_session <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "careers session") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "careers session") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "careers session") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "careers session"),
  1,
  0)
MY499_data$singled_out <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "singled out") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "singled out") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "singled out") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "singled out"),
  1,
  0)
MY499_data$other_C <- ifelse(
  (!is.na(MY499_data$Permission_1) & MY499_data$Permission_1 == "other") |
    (!is.na(MY499_data$Permission_2) & MY499_data$Permission_2 == "other") |
    (!is.na(MY499_data$Permission_3) & MY499_data$Permission_3 == "other") |
    (!is.na(MY499_data$Permission_4) & MY499_data$Permission_4 == "other"),
  1,
  0)

#Re-ordering data frame
MY499_data <- MY499_data %>%
  relocate(condition, .before = 1) %>%
  relocate(consent, .before = 2) %>%
  relocate(class, .before = 3) %>%
  relocate(ses_self_report, .before = 4) %>%
  relocate(income_factor, .before = 5) %>%
  relocate(median_income, .before = 6) %>%
  relocate(employment_factor, .before = 7) %>%
  relocate(Age, .before = 8) %>%
  relocate(sex_dummy, .before = 9) %>%
  relocate(ethnicity_simple, .before = 10) %>%
  relocate(immigration_dummy, .before = 11) %>%
  relocate(language_dummy, .before = 12) %>%
  relocate(no_children, .before = 13) %>%
  relocate(child_age, .before = 14) %>%
  relocate(fsm_dummy, .before = 15) %>%
  relocate(confidence_dummy, .before = 16) %>%
  relocate(no_activities, .before = 17) %>%
  relocate(understand_score, .before = 18) %>%
  relocate(pis_minutes, .before = 19) %>%
  relocate(duration_minutes, .before = 20) %>%
  relocate(personal_data, .before = 21) %>%
  relocate(no_benefit, .before = 22) %>%
  relocate(gender_refuse, .before = 23) %>%
  relocate(pressure, .before = 24) %>%
  relocate(mistrust_research, .before = 25) %>%
  relocate(child_preference_R, .before = 26) %>%
  relocate(misunderstanding, .before = 27) %>%
  relocate(effort, .before = 28) %>%
  relocate(school_involvement, .before = 29) %>%
  relocate(SEN, .before = 30) %>%
  relocate(lack_information, .before = 31) %>%
  relocate(other_R, .before = 32) %>%
  relocate(no_harms, .before = 33) %>%
  relocate(value_research, .before = 34) %>%
  relocate(personal_benefit, .before = 35) %>%
  relocate(data_compliance, .before = 36) %>%
  relocate(not_intrusive, .before = 37) %>%
  relocate(trust_school, .before = 38) %>%
  relocate(trust_LSE, .before = 39) %>%
  relocate(full_information, .before = 40) %>%
  relocate(child_preference_C, .before = 41) %>%
  relocate(careers_session, .before = 42) %>%
  relocate(singled_out, .before = 43) %>%
  relocate(other_C, .before = 44) %>%
  relocate(understand_correct, .before = 45) %>%
  relocate(understand_1_correct, .before = 46) %>%
  relocate(understand_2_correct, .before = 47) %>%
  relocate(understand_3_correct, .before = 48) %>%
  relocate(understand_4_correct, .before = 49) %>%
  relocate(participant_id, .before = 50)
str(MY499_data)

#Extracting cleaned data
MY499_clean <- MY499_data[, 1:50]
str(MY499_clean)

##Imputing missing data
MY499_clean$employment_factor <- na_if(MY499_clean$employment_factor, "Missing")
table(is.na(MY499_clean$employment_factor))
MY499_clean <- MY499_clean %>%
  relocate(participant_id, .before = 7)
MY499_impute <- MY499_clean[, 7:26]
md.pattern(MY499_impute)

#default
imp_default <- mice(MY499_impute, m = 10, seed = 123)
plot(imp_default, y = "employment_factor")
stripplot(imp_default, data = "employment_factor", cex=1)
#pmm 
imp_pmm <- mice(MY499_impute, m = 10, defaultMethod = c("pmm", "logreg", "pmm", "polr"), seed = 123)
plot(imp_pmm, y = "employment_factor")
stripplot(imp_pmm, data = "employment_factor", cex=1)
imp_pmm_complete <- complete(imp_pmm)
table(is.na(imp_pmm_complete$employment_factor))
table(imp_pmm_complete$employment_factor)
#cart 
imp_cart <- mice(MY499_impute, m = 10, defaultMethod = c("pmm", "logreg", "cart", "polr"), seed = 123)
plot(imp_cart, y = "employment_factor")
stripplot(imp_cart, data = "employment_factor", cex=1)
#Merging imputed data
imp_pmm_complete <- imp_pmm_complete %>%
  rename(employment_imputed = employment_factor)
imp_employment_only <- imp_pmm_complete[, c(1, 2)]
imp_employment <- merge(MY499_clean, imp_employment_only, by = "participant_id", all = TRUE)

##Results
total_freq_table <- table(MY499_clean$condition, MY499_clean$consent)
total_prop_table <- prop.table(total_freq_table, margin = 1)
print(total_prop_table)

#primary analysis
model1 <- lm_robust(consent ~ condition, data = MY499_clean)
summary(model1)
model2 <- lm_robust(consent ~ condition * class, data = MY499_clean)
summary(model2)

#subgroup analysis
subgroup_age <- lm_robust(consent ~ condition * Age, data = MY499_clean)
summary(subgroup_age)
subgroup_sex <- lm_robust(consent ~ condition * sex_dummy, data = MY499_clean)
summary(subgroup_sex)
subgroup_ethnicity <- lm_robust(consent ~ condition * ethnicity_simple, data = MY499_clean)
summary(subgroup_ethnicity)
subgroup_immigration <- lm_robust(consent ~ condition * immigration_dummy, data = MY499_clean)
summary(subgroup_immigration)
subgroup_language <- lm_robust(consent ~ condition * language_dummy, data = MY499_clean)
summary(subgroup_language)
subgroup_employment <- lm_robust(consent ~ condition * employment_imputed, data = imp_employment)
summary(subgroup_employment)
subgroup_employment_class <- lm_robust(consent ~ condition * employment_imputed + class, data = imp_employment)
summary(subgroup_employment_class)
subgroup_childage <- lm_robust(consent ~ condition * child_age, data = MY499_clean)
summary(subgroup_childage)
subgroup_nochildren <- lm_robust(consent ~ condition * no_children, data = MY499_clean)
summary(subgroup_nochildren)

##manipulation checks
#no benefit
nobenefit_freq_table <- table(MY499_clean$condition[MY499_clean$consent == 0], MY499_clean$no_benefit [MY499_clean$consent == 0])
nobenefit_prop_table <- prop.table(nobenefit_freq_table, margin = 1)
print(nobenefit_prop_table)
lm_nobenefit <- lm_robust(no_benefit ~ condition, data = MY499_clean)
summary(lm_nobenefit)
#personal benefit
persbenefit_freq_table <- table(MY499_clean$condition[MY499_clean$consent == 1], MY499_clean$personal_benefit [MY499_clean$consent == 1])
persbenefit_prop_table <- prop.table(persbenefit_freq_table, margin = 1)
print(persbenefit_prop_table)
lm_persbenefit <- lm_robust(personal_benefit ~ condition, data = MY499_clean)
summary(lm_persbenefit)
#personal data
persdata_freq_table <- table(MY499_clean$condition[MY499_clean$consent == 0], MY499_clean$personal_data [MY499_clean$consent == 0])
persdata_prop_table <- prop.table(persdata_freq_table, margin = 1)
print(persdata_prop_table)
lm_persdata <- lm_robust(personal_data ~ condition, data = MY499_clean)
summary(lm_persdata)
#mistrust research
mistrust_freq_table <- table(MY499_clean$condition[MY499_clean$consent == 0], MY499_clean$mistrust_research [MY499_clean$consent == 0])
mistrust_prop_table <- prop.table(mistrust_freq_table, margin = 1)
print(mistrust_freq_table)
print(mistrust_prop_table)
#value research
lm_valueresearch <- lm_robust(value_research ~ condition, data = MY499_clean)
summary(lm_valueresearch)
#no harm
lm_noharms <- lm_robust(no_harms ~ condition, data = MY499_clean)
summary(lm_noharms)
#child preference
lm_childpref <- lm_robust(child_preference_R ~ condition, data = MY499_clean)
summary(lm_childpref)

###Robustness checks
##Balance tests
enrichment_data <- subset(MY499_data, !is.na(enrichment_condition))
simple_data <- subset(MY499_data, !is.na(simple_condition))
#Class
t.test(class ~ enrichment_dummy, data = enrichment_data)
t.test(class ~ simple_dummy, data = simple_data)
#FSM
t.test(fsm_dummy ~ enrichment_dummy, data = enrichment_data)
t.test(fsm_dummy ~ simple_dummy, data = simple_data)
#Age
t.test(Age ~ enrichment_dummy, data = enrichment_data)
t.test(Age ~ simple_dummy, data = simple_data)
#Sex
t.test(sex_dummy ~ enrichment_dummy, data = enrichment_data)
t.test(sex_dummy ~ simple_dummy, data = simple_data)
#Ethnicity
t.test(ethnicity_simple ~ enrichment_dummy, data = enrichment_data)
t.test(ethnicity_simple ~ simple_dummy, data = simple_data)
#Number of children
t.test(no_children ~ enrichment_dummy, data = enrichment_data)
t.test(no_children ~ simple_dummy, data = simple_data)
#Immigration
t.test(immigration_dummy ~ enrichment_dummy, data = enrichment_data)
t.test(immigration_dummy ~ simple_dummy, data = simple_data)
#Main language
t.test(language_dummy ~ enrichment_dummy, data = enrichment_data)
t.test(language_dummy ~ simple_dummy, data = simple_data)

##Class associations
cor(MY499_clean[, c("class", "fsm_dummy", "median_income", "ses_self_report")], use = "complete.obs")
#Number activities
activities_class <- lm_robust(no_activities ~ class, data = MY499_clean)
summary(activities_class)
activities_fsm <- lm_robust(no_activities ~ fsm_dummy, data = MY499_clean)
summary(activities_fsm)
activities_median <- lm_robust(no_activities ~ median_income, data = MY499_clean)
summary(activities_median)
activities_ses <- lm_robust(no_activities ~ ses_self_report, data = MY499_clean)
summary(activities_ses)
#Confidence levels
confidence_class <- lm_robust(confidence_dummy ~ class, data = MY499_clean)
summary(confidence_class)
confidence_fsm <- lm_robust(confidence_dummy ~ fsm_dummy, data = MY499_clean)
summary(confidence_fsm)
confidence_median <- lm_robust(confidence_dummy ~ median_income, data = MY499_clean)
summary(confidence_median)
confidence_ses <- lm_robust(confidence_dummy ~ ses_self_report, data = MY499_clean)
summary(confidence_ses)
#Model 2 with other class variables
class_model2 <- lm_robust(consent ~ condition * class, data = MY499_clean)
summary(class_model2)
fsm_model2 <- lm_robust(consent ~ condition * fsm_dummy, data = MY499_clean)
summary(fsm_model2)
ses_model2 <- lm_robust(consent ~ condition * ses_self_report, data = MY499_clean)
summary(ses_model2)
income_model2 <- lm_robust(consent ~ condition * median_income, data = MY499_clean)
summary(income_model2)

##Mechanisms
#Understanding level 
understanding_consent <- lm_robust(understand_score ~ condition, data = MY499_clean)
summary(understanding_consent)
understanding_consent_model2 <- lm_robust(consent ~ condition * understand_score, data = MY499_clean)
summary(understanding_consent_model2)
#Duration and consent
duration_consent <- lm_robust(duration_minutes ~ condition, data = MY499_clean)
summary(duration_consent)
duration_consent_model2 <- lm_robust(consent ~ condition * duration_minutes, data = MY499_clean)
summary(duration_consent_model2)
pis_consent <- lm_robust(pis_minutes ~ condition, data = MY499_clean)
summary(pis_consent)
pis_consent_model2 <- lm_robust(consent ~ condition * pis_minutes, data = MY499_clean)
summary(pis_consent_model2)

##Additional robustness checks
#Logistic regression
logistic_model1 <- glm(consent ~ condition, family = "binomial", data = MY499_clean)
summary(logistic_model1)
logistic_model2 <- glm(consent ~ condition * class, family = "binomial", data = MY499_clean)
summary(logistic_model2)
#Understanding scores
sumunderstanding_correct <- lm_robust(understand_correct ~ condition, data = MY499_clean)
summary(sumunderstanding_correct)
sumunderstanding_correct_model2 <- lm_robust(consent ~ condition * understand_correct, data = MY499_clean)
summary(sumunderstanding_correct_model2)
understandingq1_correct <- lm_robust(understand_1_correct ~ condition, data = MY499_clean)
summary(understandingq1_correct)
understandingq1_correct_model2 <- lm_robust(consent ~ condition * understand_1_correct, data = MY499_clean)
summary(understandingq1_correct_model2)
understandingq2_correct <- lm_robust(understand_2_correct ~ condition, data = MY499_clean)
summary(understandingq2_correct)
understandingq2_correct_model2 <- lm_robust(consent ~ condition * understand_2_correct, data = MY499_clean)
summary(understandingq2_correct_model2)
understandingq3_correct <- lm_robust(understand_3_correct ~ condition, data = MY499_clean)
summary(understandingq3_correct)
understandingq3_correct_model2 <- lm_robust(consent ~ condition * understand_3_correct, data = MY499_clean)
summary(understandingq3_correct_model2)
understandingq4_correct <- lm_robust(understand_4_correct ~ condition, data = MY499_clean)
summary(understandingq4_correct)
understandingq4_correct_model2 <- lm_robust(consent ~ condition * understand_4_correct, data = MY499_clean)
summary(understandingq4_correct_model2)
#Employment complete case
print(MY499_clean$employment_factor)
subgroup_employment_complete <- lm_robust(consent ~ condition * employment_factor, data = MY499_clean)
summary(subgroup_employment_complete)
subgroup_employment_class_complete <- lm_robust(consent ~ condition * employment_factor + class, data = MY499_clean)
summary(subgroup_employment_class_complete)
