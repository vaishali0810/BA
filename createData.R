### Packages download
library(haven)
library(dplyr)
library(forcats)
library(stringr)
library(mice)
library(checkmate)
devtools::install_github("ben-aaron188/rgpt3", force = TRUE)
library(rgpt3)
library(openai)
library(readr)

# Load GESIS Data
ZA7650 <- read_dta("ZA7650_v2-0-0.dta")

# India Subset
india <- as.data.frame(subset(ZA7650, country == 356))
india <- india[, c(
       "SEX", "BIRTH", "AGE", "EDUCYRS", "EDULEVEL", "WORK", "IN_ISCD", "ISCED", "WRKHRS",
       "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9",
       "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21",
       "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "v32", "v33", "v34",
       "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42", "v43", "v44", "v45", "v46", "v47",
       "v48", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58", "v59",
       "v60",
       "EMPREL", "WRKSUP", "NSUP", "TYPORG1", "TYPORG2", "ISCO08", "MAINSTAT", "PARTLIV", "SPWORK",
       "SPWRKHRS", "SPEMPREL", "SPWRKSUP", "SPISCO08", "SPMAINST", "UNION", "IN_RELIG", "RELIGGRP", "ATTEND", "TOPBOT", "VOTE_LE", "IN_PRTY",
       "PARTY_LR",
       "IN_ETHN1", "IN_ETHN2", "HOMPOP",
       "HHADULT", "HHCHILDR", "HHTODD", "IN_RINC","IN_INC", "MARITAL", "F_BORN", "M_BORN", "URBRURAL", "IN_REG",
       "CASEID", "SUBSCASE", "DATEYR",
       "DATEMO", "DATEDY", "INTLANG", "WEIGHT", "MODE", "PARTIALS")]

# Subset to India Political Data
india_political <- india[, c( "AGE", "SEX", "EDULEVEL", "WORK", "IN_RINC", "URBRURAL", "IN_REG","IN_ETHN1","IN_RELIG", "PARTY_LR","VOTE_LE", "IN_PRTY", "v4",
  "v10", "v14","MAINSTAT", "TOPBOT", "WEIGHT" )]

## Fix the data frame (1311 entries)
# Age after 18 only for voter
india_political <- india_political %>% filter(AGE >= 18, VOTE_LE == 1)  %>%
                   select(-VOTE_LE)

# Age
india_political <- india_political %>% 
  mutate (Age = AGE) %>%
  select(-AGE)

# sex
india_political <- india_political %>% 
  mutate(Sex = case_when(
    SEX == 1 ~ "male",
    SEX == 2 ~ "female",
    TRUE ~ as.character(SEX)  # Keep other values as they are
  )) %>%
  select(-SEX)

# Educaion Level
india_political <- india_political %>%
  mutate(Education = case_when(
    EDULEVEL == -9 ~ NA,
    EDULEVEL == 0 ~ "no education",
    EDULEVEL == 1 ~ "primary education",
    EDULEVEL == 2 ~ "lower secondary education",
    EDULEVEL == 3 ~ "upper secondary education",
    EDULEVEL == 4 ~ "post secondary",
    EDULEVEL == 5 ~ "short-cycle tertiary",
    EDULEVEL == 6 ~ "bachelor",
    EDULEVEL == 7 ~ "master",
    EDULEVEL == 8 ~ "Phd",
    TRUE ~ as.character(EDULEVEL)  # Keep other values as they are
  ))%>%
  select(-EDULEVEL)


# Work
india_political <- india_political %>%
  mutate(Work = case_when(
    WORK == 1 ~ "currently",
    WORK == 2 ~ "formerly",
    WORK == 3 ~ "never",
    TRUE ~ as.character(WORK)  # Keep other values as they are
  )) %>%
  select(-WORK)

# Employment Status
india_political <- india_political %>%
  mutate(EmploymentStatus = case_when(
   MAINSTAT == 1 ~ "in paid work",
     MAINSTAT == 2 ~ "unemployed and looking for a job",
     MAINSTAT == 3 ~ "in education",
     MAINSTAT == 4 ~ "apprentice",
     MAINSTAT == 5 ~ "permanently sick",
     MAINSTAT == 6 ~ "retired",
     MAINSTAT == 7 ~ "in domestic work",
     MAINSTAT == 8 ~ "in compulsory military service or community service",
     MAINSTAT == 9 ~ "Other",
    TRUE ~ as.character( MAINSTAT)  # Keep other values as they are
  )) %>%
  select(- MAINSTAT)

# Income
india_political <- india_political %>% 
  mutate (Income = IN_RINC)%>%
  select(-IN_RINC)

# Living
india_political <- india_political %>%
  mutate(Living = case_when(
    URBRURAL == 1 ~ "a big city",
    URBRURAL == 2 ~ "the suburbs",
    URBRURAL == 3 ~ "town",
    URBRURAL == 4 ~ "country village",
    URBRURAL == 5 ~ "a farm in the country",
    TRUE ~ as.character(URBRURAL)  # Keep other values as they are
  )) %>%
  select(-URBRURAL)

# state
india_political <- india_political %>%
  mutate(State = case_when(
   IN_REG == 1  ~ "Andhra Pradesh",
   IN_REG == 2  ~ "Arunachal Pradesh",
   IN_REG == 3  ~ "Assam",
   IN_REG == 4  ~ "Bihar",
   IN_REG == 5  ~ "Chhattisgarh",
   IN_REG == 6  ~ "Goa",
   IN_REG == 7  ~ "Gujarat",
   IN_REG == 8  ~ "Haryana",
   IN_REG == 9  ~ "Himachal Pradesh",
   IN_REG == 10 ~ "Jammu and Kashmir",
   IN_REG == 11 ~ "Jharkhand",
   IN_REG == 12 ~ "Karnataka",
   IN_REG == 13 ~ "Kerala",
   IN_REG == 14 ~ "Madhya Pradesh",
   IN_REG == 15 ~ "Maharashtra",
   IN_REG == 16 ~ "Manipur",
   IN_REG == 17 ~ "Meghalaya",
   IN_REG == 18 ~ "Mizoram",
   IN_REG == 19 ~ "Nagaland",
   IN_REG == 20 ~ "Odisha",
   IN_REG == 21 ~ "Punjab",
   IN_REG == 22 ~ "Rajasthan",
   IN_REG == 23 ~ "Sikkim",
   IN_REG == 24 ~ "Tamil Nadu",
   IN_REG == 25 ~ "Telangana",
   IN_REG == 26 ~ "Tripura",
   IN_REG == 27 ~ "Uttarakhand",
   IN_REG == 28 ~ "Uttar Pradesh",
   IN_REG == 29 ~ "West Bengal",
   IN_REG == 30 ~ "Andaman and Nicobar Islands",
   IN_REG == 31 ~ "Chandigarh",
   IN_REG == 32 ~ "Dadra and Nagar Haveli",
   IN_REG == 33 ~ "Daman and Diu",
   IN_REG == 34 ~ "Delhi",
   IN_REG == 35 ~ "Lakshadweep",
   IN_REG == 36 ~ "Puducherry",
    TRUE ~ as.character(IN_REG)  # If none of the conditions match, keep the original value
  )) %>%
  select(-IN_REG)

# Religion
india_political <- india_political %>%
  mutate(Religion = case_when(
    IN_RELIG == 1 ~ "Catholic",
    IN_RELIG == 2 ~ "Protestant",
    IN_RELIG == 3 ~ "Orthodox",
    IN_RELIG == 4 ~ "Other Christian",
    IN_RELIG == 5 ~ "Jewish",
    IN_RELIG == 6 ~ "Muslim",
    IN_RELIG == 7 ~ "Buddhist",
    IN_RELIG == 8 ~ "Hindu",
    IN_RELIG == 9 ~ "Sikh",
    IN_RELIG == 10 ~ "Other religion",
    IN_RELIG == 0 ~ "No religion",
    TRUE ~ as.character(IN_RELIG)  # Keep other values as they are
  ))%>%
  select(-IN_RELIG)

# Religious Caste
india_political <- india_political %>%
  mutate(ReligiousCaste = case_when(
    IN_ETHN1 == 11 ~ "SC (Scheduled Caste/ Dalits)",
    IN_ETHN1 == 12 ~ "ST (Scheduled Tribes)",
    IN_ETHN1 == 13 ~ "OBC (Other Backward Classes)",
    IN_ETHN1 == 14 ~ "UCH (Upper Caste Hindus)",
    IN_ETHN1 == 21 ~ "Shiya",
    IN_ETHN1 == 22 ~ "Sunni",
    IN_ETHN1 == 23 ~ "Others",
    IN_ETHN1 == 31 ~ "General",
    IN_ETHN1 == 32 ~ "Others",
    IN_ETHN1 == 41 ~ "General",
    IN_ETHN1 == 42 ~ "Dalit",
    IN_ETHN1 == 95 ~ "Others",
    TRUE ~ as.character(IN_ETHN1)  # Keep other values as they are
  ))%>%
  select(-IN_ETHN1)

# Political Affiliation
india_political <- india_political %>%
  mutate(PoliticalAffiliation = case_when(
    PARTY_LR == -8 ~ NA,
   PARTY_LR == -7 ~ NA,
   PARTY_LR == -4 ~ NA,
   PARTY_LR == -1 ~ NA,
   PARTY_LR == 1 ~ "Far left (communist, etc.)",
   PARTY_LR == 2 ~ "Left / center left",
   PARTY_LR == 3 ~ "Center / liberal",
   PARTY_LR == 4 ~ "Right / conservative",
   PARTY_LR == 5 ~ "Far right (fascist, etc.)",
   PARTY_LR == 6 ~ "Other",
   PARTY_LR == 96 ~ NA,
    TRUE ~ as.character(PARTY_LR)  # Keep other values as they are
  ))%>%
  select(-PARTY_LR)

# Vote
india_political <- india_political %>%
  mutate(Vote = case_when(
    IN_PRTY == -8 ~ NA,
    IN_PRTY == 1 ~ "Congress",
    IN_PRTY == 2 ~ "BJP",
    IN_PRTY == 3 ~ "Left front Parties/Communist Party",
    IN_PRTY == 4 ~ "Third Front",
    IN_PRTY == 5 ~ "Regional Party",
    IN_PRTY == 6 ~ "Other party",
    TRUE ~ as.character(IN_PRTY)  # Keep other values as they are
  ))%>%
  select(-IN_PRTY)

## Trust in government
india_political <- india_political %>%
  mutate(trust = case_when(
    v14 == -9 ~ NA,
    v14 == -8 ~ NA,
    v14 == 0 ~ "no trust",
    v14 == 1 ~ "little trust",
    v14 == 2 ~ "little trust",
    v14 == 3 ~ "little trust",
    v14 == 4 ~ "moderate trust",
    v14 == 5 ~ "moderate trust",
    v14 == 6 ~ "moderate trust",
    v14 == 7 ~ "trust",
    v14 == 8 ~ "trust",
    v14 == 9 ~ "trust",
    v14 == 10 ~ "complete trust",
  ))%>%
  select(-v14)

# Rest
india_political <- india_political %>%
  mutate(V4 = v4, V10 = v10, Selfplacement= TOPBOT, Weight = WEIGHT )%>%
  select(-c(v4, v10, TOPBOT, WEIGHT)) %>%
  mutate(lfdn = row_number(), .before = 1)

# Remove all the rows where there the Political Affiliation is not known --> hard to mutate since the vote is also unknown ---> n = 1113
india_new <- india_political
india_new <- india_new[complete.cases(india_new$PoliticalAffiliation), ]

# Create Prompt
india_new <- india_new %>%
  mutate(
    prompt = paste0("I am ", Age, " and I am a ", Sex,". ",
                    "I have a ", Education, ". ",
                    "I am ", EmploymentStatus,". ",
                    "My personal income is about ", Income, ". ",
                    "I am a ", Religion, ". ",
                    "I live in a ", Living, " in ", State, ". ",
                    "Ideologically, I am leaning ", PoliticalAffiliation, ". ",
                    "I identify with ", Vote, ". ",
                    "I have ", trust, " in the government. ",
                    "Did I vote in the Indian General Election (Lok Shabha) in 2019, and if so, which party did I vote for? 
                    (Please use this information to make a prediction as well as possible, and I would like the answer to be extremly short). I [INSERT]"))

write.csv(india_new, file = "india_new.csv")

###########################################################################################################
