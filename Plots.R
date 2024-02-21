knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(nnet)
library(ggstats)
library(ggtext)
library(coefplot)
library(stargazer)
library(vtable)
library(ggh4x)
library(marginaleffects)
options(scipen = 999)

# Load Final dataset
load("GPT3_data_complete_2019.Rdata")

mc_india_final <- mc_india_final %>%
  mutate(sample_no = ifelse(!is.na(completion.1), 1,
                            ifelse(!is.na(completion.2), 2, 0)))

################################################# Data Transformation ##########################################################
# For regression analyses:
data_final_factors <- mc_india_final %>%
  mutate(
    Sex = factor(Sex,
                    levels = c("male","female"),
                    labels = c("male","female")),
    Education = factor(Education,
                 levels = c("no education",
                            "primary education",
                            "lower secondary education",
                            "upper secondary education",
                            "post secondary",
                            "bachelor",
                            "master"), 
                 labels = c("no education",
                            "primary education",
                            "lower secondary education",
                            "upper secondary education",
                            "post secondary",
                            "bachelor",
                            "master"),
                 ordered = FALSE),
    Work = factor(Work,
                 levels = c("never",
                            "formerly",
                            "currently"),
                 labels = c("never",
                            "formerly",
                            "currently")),
    Income = factor(Income,
                      levels = c(1500, 4500, 8000, 15000, 35000, 75000, -9), 
                      labels = c("low", "medium","above average", "high", "very high", "highest", NA),
                      ordered = FALSE),
    State = factor(State,
                  levels = c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh",
                             "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir",
                             "Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra",
                             "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Odisha", "Punjab",
                             "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", "Uttarakhand",
                             "Uttar Pradesh", "West Bengal", "Andaman and Nicobar Islands", "Chandigarh",
                             "Dadra and Nagar Haveli", "Daman and Diu", "Delhi", "Lakshadweep", "Puducherry"),
                  labels = c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh",
                             "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir",
                             "Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra",
                             "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Odisha", "Punjab",
                             "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", "Uttarakhand",
                             "Uttar Pradesh", "West Bengal", "Andaman and Nicobar Islands", "Chandigarh",
                             "Dadra and Nagar Haveli", "Daman and Diu", "Delhi", "Lakshadweep", "Puducherry")),
    Religion = factor(Religion,
                       levels = c("Hindu", "No religion", "Muslim" , "Sikh" , "Catholic", "Other Christian" ,"Buddhist" ), 
                       labels = c("Hindu", "No religion", "Muslim" , "Sikh" , "Catholic", "Other Christian" ,"Buddhist"),
                       ordered = FALSE),
    PoliticalAffiliation = factor(PoliticalAffiliation,
                       levels = c("Right / conservative","Left / center left" , "Center / liberal" ,"Far left (communist, etc.)", "Other"),
                       labels = c("Right / conservative","Left / center left" , "Center / liberal" ,"Far left (communist, etc.)", "Other"),
                       ordered = FALSE),
  Vote = factor(Vote,
                     levels = c("BJP", 
                                "Congress", 
                                "Regional Party" , 
                                "Left front Parties/Communist Party",
                                "Third Front", 
                                "Other party"),
                     labels = c("BJP", 
                                "Congress", 
                                "Regional Party" , 
                                "Left front Parties/Communist Party",
                                "Third Front", 
                                "Other party")),
  trust = factor(trust,
                 levels = c("complete trust", "trust", "little trust", "moderate trust", NA, "no trust"),
                 labels = c("complete trust", "trust", "little trust", "moderate trust", "no trust"),
                 ordered = FALSE),

    vote_gpt = factor(vote_gpt,
                      levels = c("BJP", 
                                 "Congress", 
                                 "Regional Party" , 
                                 "Left front Parties/Communist Party",
                                 "Third Front", 
                                 "Other party",
                                 "Unknown",
                                 "No vote"),
                      labels = c("BJP", 
                                 "Congress", 
                                 "Regional Party" , 
                                 "Left front Parties/Communist Party",
                                 "Third Front", 
                                 "Other party",
                                 "Unknown",
                                 "No vote"))
  )
# Transform variables with 5 categories to numeric
data_final_numeric <- data_final_factors %>%
  mutate(
    Education = ifelse(Education == "no education", 0,
                ifelse(Education == "primary education", 1,
                ifelse(Education == "lower secondary education", 2,
                ifelse(Education == "upper secondary education", 3,
                ifelse(Education == "post secondary", 4,
                ifelse(Education == "bachelor", 5,
                ifelse(Education == "master", 6,  
                NA))))))),
   PoliticalAffiliation = ifelse(PoliticalAffiliation == "Far left (communist, etc.)", -2,
                          ifelse(PoliticalAffiliation == "Left / center left", -1,
                          ifelse(PoliticalAffiliation == "Center / liberal", 0,
                          ifelse(PoliticalAffiliation == "Right / conservative", 1,
                          ifelse(PoliticalAffiliation == "Other", 5,
                                                   NA)))))
  )

# Removing respondents that have at least one NA in gpt_vote (for comparability between samples)
data_final_numeric_noNA <- data_final_numeric %>%
  drop_na("vote_gpt")

####################################### Figure 3: Frequency of Matches in Overall Sample #################################
  
### Tables
# Overall
table_matches_overall_c <- table(mc_india_final$match_vote_outcome)
table_matches_overall_s <- prop.table(table_matches_overall_c)

data_matches_overall_s <- as.data.frame(table_matches_overall_s)
data_matches_overall_s <- data_matches_overall_s %>%
  mutate(sample = "Overall") %>%
  rename(match = Var1)

# Plot: 
plot_overall_match <- ggplot(data_matches_overall_s, aes(x = sample, y = Freq, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Freq*100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "black") +
  xlab("Sample") +
  ylab("Frequency") +
  scale_fill_manual(values = c("#012340", "#03A63C"),
                    labels = c("Different", "Match"),
                    name = "GPT Prediction") +
  ggtitle("Frequency of Matches in Overall Sample") +
  theme_minimal()

plot_overall_match
ggsave("overall_match.png", plot_overall_match, height = 6, width = 8)

################################### Figure 4: Distribution of vote shares as estimated by GESIS and GPT- 3.5 ##########################################
# Plot vote compare between various parties:
## Variance Estimation for GPT Votes
sample_proportions <- mc_india_final %>%
  group_by(vote_gpt) %>%
  summarise(n = n(), .groups = "keep") %>%
  ungroup(vote_gpt) %>%
  mutate(prop_estimate = n / sum(n)) %>%
  mutate(prop_variance = prop_estimate * (1 - prop_estimate) / sum(n))

sample_proportions_df <- sample_proportions %>%
  select(vote_gpt, prop_estimate, prop_variance) %>%
  rename(party = vote_gpt) %>%
  rename(freq = prop_estimate) %>%
  replace_na(list(party = "no prediction")) %>%
  mutate(source = "GPT") %>%
  mutate(prop_variance = sqrt(freq * (1-freq)/1113))%>%
  mutate(source = factor(source, levels = c("GESIS", "GPT")))

## Variance Estimation for Human Votes
sample_proportions_human <- mc_india_final %>%
  group_by(Vote) %>%
  summarise(n = n(), .groups = "keep") %>%
  ungroup(Vote) %>%
  mutate(prop_estimate = n / sum(n)) %>%
  mutate(prop_variance = prop_estimate * (1 - prop_estimate) / sum(n))

data_vote_freq <- sample_proportions_human %>%
  select(Vote, prop_estimate, prop_variance) %>%
  rename(party = Vote) %>%
  rename(freq = prop_estimate) %>%
  replace_na(list(party = "no prediction")) %>%
  mutate(source = "GESIS") %>%
  mutate(source = factor(source, levels = c("GESIS", "GPT"))) %>%
  mutate(prop_variance = sqrt(freq * (1-freq)/1113))%>% # SE if GESIS eligible voters could be treated unweighted
  add_row(party = "No vote", freq = 0, source = "GESIS",
          prop_variance = NA)

# Merge GESIS & GPT, remove small percentage of "no prediction"
data_vote_parties <- bind_rows(sample_proportions_df, data_vote_freq)

data_vote_parties <- data_vote_parties %>%
  mutate(source = factor(source, levels = c("GESIS", "GPT"))) %>%
  mutate(party = case_when(
    party == "Left front Parties/Communist Party" ~ "Left Front/Communist Party",
    party == "Other party" ~ "Other Party",
    TRUE ~ party  # Keep other values unchanged
  )) %>%
  mutate(party = ifelse(party == "BJP", "BJP",
                 ifelse(party == "Congress", "Congress",
                 ifelse(party == "Left Front/Communist Party", "Left Front/Communist Party",
                 ifelse(party == "Third Front", "Third Front",
                 ifelse(party == "Regional Party", "Regional Party",
                 ifelse(party == "No vote", "No vote",
                 ifelse(party == "Other Party", "Other Party", NA)))))))) %>%
  mutate(party = as.factor(party))

# Plot: 
plot_shares_aggregate <- ggplot(data_vote_parties, aes(x = party, y = freq, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(freq*100,1),
                y = freq + (1.96*prop_variance) + 0.005), 
                position = position_dodge(0.9),
                vjust = 0,
                color = "#012340") +
  xlab("") + 
  ylab("") +
  scale_x_discrete(limits = c("BJP", "Congress", "Left Front/Communist Party", "Third Front", "Regional Party", "Other Party", "No vote")) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(limits = c("GESIS", "GPT"),
                    values = c("GESIS" = "#03A63C", "GPT" = "#FAA32B"),
                    labels = c("GESIS",
                               "GPT-3.5"),
                    name = "Reported vote choice \naccording to") +
  geom_errorbar(aes(x = party,
                    ymin = freq - (1.96*prop_variance),
                    ymax = freq + (1.96*prop_variance)),
                    width = 0.4,
                    colour = "#012340",
                    position = position_dodge(width = 0.9)) +
  ggtitle("") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title.align = 1,
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

plot_shares_aggregate

ggsave("shares_aggregate.png", plot_shares_aggregate, height = 4, width = 8)

################################################ Figure 5: F1-Scores ############################################

table_matches_parties_s <- prop.table(table(mc_india_final$vote, mc_india_final$match_vote_outcome))
table_matches_gpt_s <- prop.table(table(mc_india_final$vote_gpt, mc_india_final$match_vote_outcome))

data_matches_gpt_s <- as.data.frame(table_matches_gpt_s)
data_matches_gpt_s <- data_matches_gpt_s %>%
  rename(match = Var2) %>%
  rename(party = Var1)

data_matches_parties_s <- as.data.frame(table_matches_parties_s)
data_matches_parties_s <- data_matches_parties_s %>%
  rename(match = Var2) %>%
  rename(party = Var1)

data_matches_overall_parties_s <- data_matches_overall_s %>%
  rename(party = sample) %>%
  mutate(party = factor(party))

data_matches_gpt_all <- bind_rows(data_matches_overall_parties_s, data_matches_gpt_s) %>%
  mutate(party = ifelse(party == "BJP", "BJP",
                 ifelse(party == "Congress", "Congress",
                 ifelse(party == "Other Party", "Other Party",
                 ifelse(party == "Left Front/Communist Party", "Left Front/Communist Party",
                 ifelse(party == "No vote", "No vote",
                 ifelse(party == "Regional Party", "Regional Party",
                 ifelse(party == "Third Front", "Third Front",
                 ifelse(party == "Unknown", "Unknown",
                 ifelse(party == "Overall", "Overall", party))))))))))

data_matches_parties_all <- bind_rows(data_matches_overall_parties_s, data_matches_parties_s) %>%
  mutate(party = ifelse(party == "BJP", "BJP",
                        ifelse(party == "Congress", "Congress",
                        ifelse(party == "Other Party", "Other Party",
                        ifelse(party == "Left Front/Communist Party", "Left Front/Communist Party",
                        ifelse(party == "No vote", "No vote",
                        ifelse(party == "Regional Party", "Regional Party",
                        ifelse(party == "Third Front", "Third Front",
                        ifelse(party == "Unknown", "Unknown",
                        ifelse(party == "Overall", "Overall", party))))))))))

data_precision <- data_matches_gpt_all %>%
  filter(match == TRUE) %>%
  rename(precision = Freq) %>%
  select(precision, party)

data_recall <- data_matches_parties_all %>%
  filter(match == TRUE) %>%
  rename(recall = Freq) %>%
  select(recall, party)

data_f1 <- merge(data_precision, data_recall, by = "party")

data_f1 <- data_f1 %>%
  mutate(f1 = (2*precision*recall) / (precision + recall))

data_f1 # F1-Scores

######################################### Figure 6: Logistic Regression on match between GPT and GESIS vote choice#########################

data_final_numeric_noNA <- data_final_numeric_noNA %>%
  mutate(State = case_when(
    State %in% c("Andhra Pradesh", "Telangana", "Karnataka", "Tamil Nadu", "Kerala", "Puducherry", "Andaman and Nicobar Islands", "Lakshwadeep") ~ "South",
    State %in% c("Gujarat", "Maharashtra", "Goa", "Rajasthan", "Daman and Diu", "Dadra and Nagar Haveli") ~ "West",
    State %in% c("Assam", "Arunachal Pradesh", "Meghalaya", "Nagaland", "Manipur", "Mizoram", "Tripura", "Sikkim", "Odisha", "West Bengal") ~ "East",
    State %in% c("Uttar Pradesh", "Bihar", "Jharkhand", "Chhattisgarh", "Madhya Pradesh", "Haryana", "Delhi", "Jammu and Kashmir", 
                 "Himachal Pradesh", "Punjab", "Uttarakhand", "Chandigarh") ~ "North"
  ))

data_final_numeric_noNA <- data_final_numeric_noNA %>%
  mutate(State = as.factor(State),
         Vote = as.factor(Vote),
         Sex = as.factor(Sex),
         Education = as.factor(Education),
         Work = as.factor(Work),
         EmploymentStatus = as.factor(EmploymentStatus),
         Income = as.factor(Income),
         Living = as.factor(Living),
         Religion = as.factor(Religion),
         PoliticalAffiliation = as.factor(PoliticalAffiliation),
         trust = as.factor(trust))

## Including Variable vote_gpt: Kind of weird error (numbers)
model_match_1 <- glm(match_vote_outcome ~ Vote + Age + Sex + Education + Work + EmploymentStatus + State + Income + Living + Religion + 
                       PoliticalAffiliation + trust + vote_gpt,
                     data = data_final_numeric_noNA, family = binomial(link = logit))
## with only vote
# model_match_2 <- glm(match_vote_outcome ~ Vote + Age + Sex + Education + Work + EmploymentStatus + State + Income + Living + Religion + 
#                       PoliticalAffiliation + trust,
#                    data = data_final_numeric_noNA, family = binomial(link = logit))

## with only vote_gpt
# model_match_3 <- glm(match_vote_outcome ~ Age + Sex + Education + Work + EmploymentStatus + State + Income + Living + Religion + 
#                      PoliticalAffiliation + trust + vote_gpt,
#                   data = data_final_numeric_noNA, family = binomial(link = logit))

summary(model_match_1)
coef(model_match_1)


data_model_match_1 <- summary(model_match_1)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(variable = (ifelse(variable == "(Intercept)", "intercept", variable))) %>%
  rename(coeff = Estimate) %>%
  rename(stderr = "Std. Error") %>%
  rename(z_value = "z value") %>%
  rename(p_value = "Pr(>|z|)") %>%
  mutate(coeff = round(coeff, digits = 3)) %>%
  mutate(stderr = round(stderr, digits = 3))%>%
  mutate(p_value = round(p_value, digits = 3))%>%
  mutate(z_value = round(z_value, digits = 3))

# Export
write.csv(export_match, "data_model_match_1", row.names = F)
         
### Plot
data_plot_match <- data_model_match_1 %>%
              filter(!is.na(stderr)) %>%
              mutate(lower = coeff - 1.96 * stderr,
                     upper = coeff + 1.96 * stderr,
                     oddsratio = exp(coeff),
                     lower_or = exp(lower),
                     upper_or = exp(upper)) %>%
             mutate(sig = ifelse(lower_or > 1 | upper_or < 1, "Significant", "Not significant")) %>%
  rename(category = variable) %>%
  mutate(variable = case_when(
             category == "intercept" ~ "Intercept",
             category %in% c("VoteCongress", "VoteLeft front Parties/Communist Party", "VoteOther party", "VoteRegional Party", "VoteThird Front") ~ "GESIS-Vote (Ref.: BJP)",
             category == "Age" ~ "Age",
             category == "Sexfemale" ~ "Gender (Ref.: male)",
             category == "Education" ~ "Education",
             category %in% c("EmploymentStatusOther", "EmploymentStatusapprentice", "EmploymentStatusin domestic work", "EmploymentStatusin education", "EmploymentStatusin paid work", "EmploymentStatuspermanently sick", "EmploymentStatusretired", "EmploymentStatusunemployed and looking for a job") ~ "Employment (Ref.: in military service)",
             category %in% c("IncomeNA", "Incomeabove average", "Incomehigh", "Incomehighest", "Incomemedium") ~ "Income (Ref.: low)",
             category %in% c("StateNorth", "StateSouth", "StateWest") ~ "State (Ref.: East)",
             category %in% c("Livinga farm in the country", "Livingcountry village", "Livingthe suburbs", "Livingtown") ~ "Living (Ref.: a big city)",
             category %in% c("ReligionBuddhist", "ReligionCatholic", "ReligionMuslim", "ReligionNo religion", "ReligionOther Christian", "ReligionSikh") ~ "Religion (Ref.: Hindu)",
             category %in% c("Workcurrently", "Workformerly") ~ "Work (Ref.: Never)",
             category %in% c("trustlittle trust", "trustmoderate trust", "trustno trust", "trusttrust") ~ "Trust (Ref.: complete trust)",
             category %in% c("vote_gptCongress", "vote_gptRegional Party", "vote_gptThird Front", "vote_gptUnknown") ~ "Vote GPT (Ref.: BJP)"
           )) %>%
  mutate(category = ifelse(category == "Sexfemale", "female",
                    ifelse(category == "Education", "Education",
                    ifelse(category == "Age", "Age",
                    ifelse(category == "EmploymentStatusOther", "other",
                    ifelse(category == "EmploymentStatusapprentice", "apprentice",
                    ifelse(category == "EmploymentStatusin domestic work", "in domestic work",
                    ifelse(category == "EmploymentStatusin education", "in education",
                    ifelse(category == "EmploymentStatusin paid work", "in paid work",
                    ifelse(category == "EmploymentStatuspermanently sick", "permanently sick",
                    ifelse(category == "EmploymentStatusretired", "retired",
                    ifelse(category == "EmploymentStatusunemployed and looking for a job", "unemployed and looking for a job",
                    ifelse(category == "VoteCongress", "Congress",
                    ifelse(category == "VoteLeft front Parties/Communist Party", "Left front Parties/Communist Party",
                    ifelse(category == "VoteOther party", "Other party",
                    ifelse(category == "VoteRegional Party", "Regional Party",
                    ifelse(category == "VoteThird Front", "Third Front",
                    ifelse(category == "IncomeNA", "NA",
                    ifelse(category == "Incomeabove average", "above average",
                    ifelse(category == "Incomehigh", "high",
                    ifelse(category == "Incomehighest", "highest",
                    ifelse(category == "Incomemedium", "medium",
                    ifelse(category == "Livinga farm in the country", "a farm in the country",
                    ifelse(category == "Livingcountry village", "village",
                    ifelse(category == "Livingthe suburbs", "suburbs",
                    ifelse(category == "Livingtown", "town",
                    ifelse(category == "ReligionBuddhist", "Buddhist",
                    ifelse(category == "ReligionCatholic", "Catholic",
                    ifelse(category == "ReligionMuslim", "Muslim",
                    ifelse(category == "ReligionNo religion", "No religion",
                    ifelse(category == "ReligionOther Christian", "Other Christian",  
                    ifelse(category == "ReligionSikh", "Sikh",
                    ifelse(category == "Workcurrently", "currently",
                    ifelse(category == "Workformerly", "formerly",
                    ifelse(category == "trustlittle trust", "little trust",
                    ifelse(category == "trustmoderate trust", "moderate trust",
                    ifelse(category == "trustno trust", "no trust",
                    ifelse(category == "trusttrust", "trust",
                    ifelse(category == "StateNorth", "north",
                    ifelse(category == "StateSouth", "south",
                    ifelse(category == "StateWest", "west", 
                    ifelse(category == "vote_gptCongress", "Congress",
                    ifelse(category == "vote_gptRegional Party", "Regional Party",
                    ifelse(category == "vote_gptThird Front", "Third Front",
                    ifelse(category == "vote_gptUnknown", "Unknown",
                      category))))))))))))))))))))))))))))))))))))))))))))) %>%
           mutate(interaction_cat_var = interaction(category, variable, sep = "&")) %>%
           mutate(interaction_cat_var = factor(interaction_cat_var,
                                               levels = c("intercept&Intercept",
                                                          "Congress&GESIS-Vote (Ref.: BJP)", "Regional Party&GESIS-Vote (Ref.: BJP)", "Left front Parties/Communist Party&GESIS-Vote (Ref.: BJP)", "Third Front&GESIS-Vote (Ref.: BJP)", "Other party&GESIS-Vote (Ref.: BJP)" ,
                                                          "Age&Age",
                                                          "female&Gender (Ref.: male)",
                                                          "Education&Education",
                                                          "formerly&Work (Ref.: Never)", "currently&Work (Ref.: Never)",
                                                          "apprentice&Employment (Ref.: in military service)", "in domestic work&Employment (Ref.: in military service)",
                                                          "in education&Employment (Ref.: in military service)", 
                                                          "in paid work&Employment (Ref.: in military service)", "other&Employment (Ref.: in military service)", 
                                                          "permanently sick&Employment (Ref.: in military service)", 
                                                          "retired&Employment (Ref.: in military service)", "unemployed and looking for a job&Employment (Ref.: in compulsory military service or community service)",
                                                          "north&State (Ref.: East)", "south&State (Ref.: East)", "west&State (Ref.: East)" ,
                                                          "medium&Income (Ref.: low)", "above average&Income (Ref.: low)","high&Income (Ref.: low)", "highest&Income (Ref.: low)", "NA&Income (Ref.: low)",
                                                          "a farm in the country&Living (Ref.: a big city)", "village&Living (Ref.: a big city)" ,"suburbs&Living (Ref.: a big city)", "town&Living (Ref.: a big city)",
                                                          "No religion&Religion (Ref.: Hindu)", "Catholic&Religion (Ref.: Hindu)" ,"Muslim&Religion (Ref.: Hindu)" , "Other Christian&Religion (Ref.: Hindu)", "Sikh&Religion (Ref.: Hindu)", "Buddhist&Religion (Ref.: Hindu)",
                                                          "little trust&Trust (Ref.: complete trust)" ,"moderate trust&Trust (Ref.: complete trust)","no trust&Trust (Ref.: complete trust)" ,"trust&Trust (Ref.: complete trust)",
                                                          "Congress&Vote GPT (Ref.: BJP)", "Regional Party&Vote GPT (Ref.: BJP)", "Third Front&Vote GPT (Ref.: BJP)", "Unknown&Vote GPT (Ref.: BJP)"
                                               )
           ))
         
## Plot:          
plot_model_match <- ggplot(drop_na(data_plot_match), 
                           aes(y= interaction_cat_var,
                               x = coeff,
                               xmin = lower,
                               xmax = upper,
                               color = sig)) + # legend colored by significance
  geom_vline(xintercept = 0, linetype = "solid", linewidth = 0.2) +
  geom_point(aes(color = sig), # point estimate
             size = 2) +
  geom_pointrange(linewidth = 0.5) + # error bars
  xlim(-30, 70) + # When using pure betas and removing Vote (GLES): Invalid
  guides(y = guide_axis_nested(delim = "&")) + 
  scale_color_manual(limits = c("Significant", "Not significant"),
                     values = c("Significant" = "#03A63C",
                                "Not significant" = "#012340"),
                     labels = c("p < 0.05", "p > 0.05")) +
  theme_minimal() +
  labs(
    x = "ß (additive effect on log-odds)",
    y = "",
    title = "Determinants of Matching Vote Choice Between GESIS and GPT") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title.position = "plot"
  )

plot_model_match

ggsave("model_match.png", plot_model_match, height = 6, width = 8)

################################# Figure 7: same logistic regression "zoomed out" ########################################
## Plot without xlim
plot_model_match_1 <- ggplot(drop_na(data_plot_match), 
                           aes(y= interaction_cat_var,
                               x = coeff,
                               xmin = lower,
                               xmax = upper,
                               color = sig)) + # legend colored by significance
  geom_vline(xintercept = 0, linetype = "solid", linewidth = 0.2) +
  geom_point(aes(color = sig), # point estimate
             size = 2) +
  geom_pointrange(linewidth = 0.5) + # error bars
 
  guides(y = guide_axis_nested(delim = "&")) + 
  scale_color_manual(limits = c("Significant", "Not significant"),
                     values = c("Significant" = "#03A63C",
                                "Not significant" = "#012340"),
                     labels = c("p < 0.05", "p > 0.05")) +
  theme_minimal() +
  labs(
    x = "ß (additive effect on log-odds)",
    y = "",
    title = "Determinants of Matching Vote Choice Between GESIS and GPT") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title.position = "plot"
  )

plot_model_match_1

ggsave("model_match_1.png", plot_model_match_1, height = 6, width = 8)

################################### Figure 8: Multinominal regression on vote choice for GESIS and GPT ###################################################
# VOTE: Comparing determinants of vote choice
#Notes from https://stackoverflow.com/questions/43623076/multinomial-logit-in-r-mlogit-versus-nnet
#In nnet::multinom the model is a neural network with no hidden layers, no bias nodes and a softmax output layer.
#Maximum conditional likelihood is the method used in multinom for model fitting.

## GPT
model_vote_gpt <- multinom(vote_gpt ~ Vote + Age + Sex + Education + State + Work + EmploymentStatus + Income + Living + Religion + 
                             PoliticalAffiliation + trust, data = data_final_numeric_noNA, Hess = TRUE)
## Get coeffs & stderrs
summary(model_vote_gpt)
coef(model_vote_gpt)

## Get n: 982
nrow(residuals(model_vote_gpt))

coeff_model_vote_gpt <- summary(model_vote_gpt)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "coeff")

stderr_model_vote_gpt <- summary(model_vote_gpt)$standard.errors %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "stderr")

z_model_vote_gpt <- summary(model_vote_gpt)$coefficients/summary(model_vote_gpt)$standard.errors
# cannot transform before calculating p-values
# have to do separately because %>% doesn't work with calculation
p_model_vote_gpt <- (1 - pnorm(abs(z_model_vote_gpt), 0, 1)) * 2 # 2-tailed z-test 
p_model_vote_gpt <- p_model_vote_gpt %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "p_value")

z_model_vote_gpt <- z_model_vote_gpt %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "z_value")

data_model_vote_gpt <- coeff_model_vote_gpt %>%
  left_join(stderr_model_vote_gpt, by = c("party", "variable")) %>%
  left_join(p_model_vote_gpt, by = c("party", "variable")) %>%
  left_join(z_model_vote_gpt, by = c("party", "variable")) 

## GESIS
data_onesample <- data_final_numeric_noNA
model_vote_gles <- multinom(Vote ~ Age + Sex + Education + Work + EmploymentStatus + State + Income + Living + Religion + 
                              PoliticalAffiliation + trust, data = data_onesample, na.action = na.omit, Hess = TRUE)
summary(model_vote_gles)

# Get coeffs, std.errors & test statistics, transform into long format & store as 1 df
coeff_model_vote_gles <- summary(model_vote_gles)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "coeff")

stderr_model_vote_gles <- summary(model_vote_gles)$standard.errors %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "stderr")

z_model_vote_gles <- summary(model_vote_gles)$coefficients/summary(model_vote_gles)$standard.errors # Wald Z
# cannot transform before calculating p-values
# have to do separately because %>% doesn't work with calculation
p_model_vote_gles <- (1 - pnorm(abs(z_model_vote_gles), 0, 1)) * 2 # 2-tailed z-test 
p_model_vote_gles <- p_model_vote_gles %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "p_value")

z_model_vote_gles <- z_model_vote_gles %>%
  as.data.frame() %>%
  rownames_to_column("party") %>%
  rename(intercept = "(Intercept)") %>%
  pivot_longer(cols = !party, names_to = "variable", values_to = "z_value")

data_model_vote_gles <- coeff_model_vote_gles %>%
  left_join(stderr_model_vote_gles, by = c("party", "variable")) %>%
  left_join(p_model_vote_gles, by = c("party", "variable")) %>%
  left_join(z_model_vote_gles, by = c("party", "variable"))

# Pseudo-R2: 0.946
model_vote_gles_loglik <- nnet:::logLik.multinom(model_vote_gles)
model_0 <- multinom(Vote ~ 1, data = data_onesample)
model_0_loglik <- nnet:::logLik.multinom(model_0)
model_vote_gles_r2 <- as.numeric(1 - model_vote_gles_loglik/model_0_loglik)

## Comparing GLES and GPT models
### Data transformation
data_model_vote_gesis_merge <- data_model_vote_gles %>%
  select(!z_value)%>%
  mutate(model = "GESIS")

data_model_vote_gpt_merge <- data_model_vote_gpt %>%
  select(!z_value)%>%
  mutate(model = "GPT")

### Plot coeffs & CIs, only significant vars
data_plot_vote_compare <- bind_rows(data_model_vote_gesis_merge, data_model_vote_gpt_merge) %>%
  mutate(lower = coeff - 1.96*stderr,
         upper = coeff + 1.96*stderr,
         oddsratio = exp(coeff)) %>%
  mutate(lower_or = exp(lower),
         upper_or = exp(upper)) %>%
  mutate(sig = ifelse(p_value < 0.05, "Significant",
                      "Not significant")) %>%
  na.omit(party) %>% 
  mutate(party = factor(party,
                        levels = c("Congress", "Regional Party", "Third Front", "Left front Parties/Communist Party","Other Party", "Unknown", "No vote"))) %>%
  
  rename(category = variable) %>%
  mutate(variable = case_when(
    category == "intercept" ~ "Intercept",
    category %in% c("VoteCongress", "VoteLeft front Parties/Communist Party", "VoteOther party", "VoteRegional Party", "VoteThird Front") ~ "GESIS-Vote (Ref.: BJP)",
    category == "Age" ~ "Age",
    category == "Sexfemale" ~ "Gender (Ref.: male)",
    category == "Education" ~ "Education",
    category %in% c("EmploymentStatusOther", "EmploymentStatusapprentice", "EmploymentStatusin domestic work", "EmploymentStatusin education", "EmploymentStatusin paid work", "EmploymentStatuspermanently sick", "EmploymentStatusretired", "EmploymentStatusunemployed and looking for a job") ~ "Employment (Ref.: in military service)",
    category %in% c("IncomeNA", "Incomeabove average", "Incomehigh", "Incomehighest", "Incomemedium") ~ "Income (Ref.: low)",
    category %in% c("StateNorth", "StateSouth", "StateWest") ~ "State (Ref.: East)",
    category %in% c("Livinga farm in the country", "Livingcountry village", "Livingthe suburbs", "Livingtown") ~ "Living (Ref.: a big city)",
    category %in% c("ReligionBuddhist", "ReligionCatholic", "ReligionMuslim", "ReligionNo religion", "ReligionOther Christian", "ReligionSikh") ~ "Religion (Ref.: Hindu)",
    category %in% c("Workcurrently", "Workformerly") ~ "Work (Ref.: Never)",
    category %in% c("trustlittle trust", "trustmoderate trust", "trustno trust", "trusttrust") ~ "Trust (Ref.: complete trust)",
  )) %>%
  mutate(category = ifelse(category == "Sexfemale", "female",
                    ifelse(category == "Education", "Education",
                    ifelse(category == "Age", "Age",
                    ifelse(category == "EmploymentStatusOther", "other",
                    ifelse(category == "EmploymentStatusapprentice", "apprentice",
                    ifelse(category == "EmploymentStatusin domestic work", "in domestic work",
                    ifelse(category == "EmploymentStatusin education", "in education",
                    ifelse(category == "EmploymentStatusin paid work", "in paid work",
                    ifelse(category == "EmploymentStatuspermanently sick", "permanently sick",
                    ifelse(category == "EmploymentStatusretired", "retired",
                    ifelse(category == "EmploymentStatusunemployed and looking for a job", "unemployed and looking for a job",
                    ifelse(category == "VoteCongress", "Congress",
                    ifelse(category == "VoteLeft front Parties/Communist Party", "Left front Parties/Communist Party",
                    ifelse(category == "VoteOther party", "Other party",
                    ifelse(category == "VoteRegional Party", "Regional Party",
                    ifelse(category == "VoteThird Front", "Third Front",
                    ifelse(category == "IncomeNA", "NA",
                    ifelse(category == "Incomeabove average", "above average",
                    ifelse(category == "Incomehigh", "high",
                    ifelse(category == "Incomehighest", "highest",
                    ifelse(category == "Incomemedium", "medium",
                    ifelse(category == "Livinga farm in the country", "a farm in the country",
                    ifelse(category == "Livingcountry village", "village",
                    ifelse(category == "Livingthe suburbs", "suburbs",
                    ifelse(category == "Livingtown", "town",
                    ifelse(category == "ReligionBuddhist", "Buddhist",
                    ifelse(category == "ReligionCatholic", "Catholic",
                    ifelse(category == "ReligionMuslim", "Muslim",
                    ifelse(category == "ReligionNo religion", "No religion",
                    ifelse(category == "ReligionOther Christian", "Other Christian",  
                    ifelse(category == "ReligionSikh", "Sikh",
                    ifelse(category == "Workcurrently", "currently",
                    ifelse(category == "Workformerly", "formerly",
                    ifelse(category == "trustlittle trust", "little trust",
                    ifelse(category == "trustmoderate trust", "moderate trust",
                    ifelse(category == "trustno trust", "no trust",
                    ifelse(category == "trusttrust", "trust",
                    ifelse(category == "StateNorth", "north",
                    ifelse(category == "StateSouth", "south",
                    ifelse(category == "StateWest", "west",      
  category))))))))))))))))))))))))))))))))))))))))) %>%
  mutate(interaction_cat_var = interaction(category, variable, sep = "&")) %>%
  mutate(interaction_cat_var = factor(interaction_cat_var,
                                      levels = c("intercept&Intercept",
                                                 "Congress&GESIS-Vote (Ref.: BJP)", "Regional Party&GESIS-Vote (Ref.: BJP)", "Left front Parties/Communist Party&GESIS-Vote (Ref.: BJP)", "Third Front&GESIS-Vote (Ref.: BJP)", "Other party&GESIS-Vote (Ref.: BJP)" ,
                                                 "Age&Age",
                                                 "female&Gender (Ref.: male)",
                                                 "Education&Education",
                                                 "formerly&Work (Ref.: Never)", "currently&Work (Ref.: Never)",
                                                 "apprentice&Employment (Ref.: in military service)", "in domestic work&Employment (Ref.: in military service)",
                                                 "in education&Employment (Ref.: in military service)", 
                                                 "in paid work&Employment (Ref.: in military service)", "other&Employment (Ref.: in military service)", 
                                                 "permanently sick&Employment (Ref.: in military service)", 
                                                 "retired&Employment (Ref.: in military service)", "unemployed and looking for a job&Employment (Ref.: in compulsory military service or community service)",
                                                 "north&State (Ref.: East)", "south&State (Ref.: East)", "west&State (Ref.: East)" ,
                                                 "medium&Income (Ref.: low)", "above average&Income (Ref.: low)","high&Income (Ref.: low)", "highest&Income (Ref.: low)", "NA&Income (Ref.: low)",
                                                 "a farm in the country&Living (Ref.: a big city)", "village&Living (Ref.: a big city)" ,"suburbs&Living (Ref.: a big city)", "town&Living (Ref.: a big city)",
                                                 "No religion&Religion (Ref.: Hindu)", "Catholic&Religion (Ref.: Hindu)" ,"Muslim&Religion (Ref.: Hindu)" , "Other Christian&Religion (Ref.: Hindu)", "Sikh&Religion (Ref.: Hindu)", "Buddhist&Religion (Ref.: Hindu)",
                                                 "little trust&Trust (Ref.: complete trust)" ,"moderate trust&Trust (Ref.: complete trust)","no trust&Trust (Ref.: complete trust)" ,"trust&Trust (Ref.: complete trust)"       
                                                 
                                      )
  ))


# Export
export_vote_compare <- data_plot_vote_compare %>%
  select(party, variable, category, coeff, p_value, model) %>%
  mutate(sig = ifelse(p_value < 0.001, "***",
                      ifelse(p_value >= 0.001 & p_value < 0.01, "**",
                             ifelse(p_value >= 0.01 & p_value < 0.05, "*",
                                    NA)))) %>%
  mutate(p_value = round(p_value, digits = 3)) %>%
  pivot_wider(names_from = party,
              values_from = all_of(c("coeff",
                                     "p_value",
                                     "sig")),
              names_sep = "_")

write.csv(export_vote_compare, "model_vote_compare.csv", row.names = F)

## Plot:
plot_vote_compare <- ggplot(data_plot_vote_compare[data_plot_vote_compare$sig == "Significant",], 
                            aes(y = interaction_cat_var,
                                x = coeff,
                                xmin = lower,
                                xmax = upper,
                                color = model)) + # legend colored by model
  geom_vline(xintercept = 0,
             linetype = "solid",
             linewidth = 0.2) +
  geom_pointrange(# error bars
    linewidth = 0.5,
    shape = 1#,
  ) +
  guides(y = guide_axis_nested(delim = "&")) + 
  xlim(-30, 25) +
  scale_color_manual(limits = c("GESIS", "GPT"),
                     values = c("GESIS" = "#027333", "GPT" = "#FAA32B"),
                     labels = c("GESIS", "GPT-3.5")) +
  facet_grid(.~party) +
  theme_minimal() +
  labs(
    x = "ß (additive effect on log-odds) - only variables with p < 0.05",
    y = "",
    title = "Determinants of Vote Choice According to GESIS and GPT \n(Reference: BJP)") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title.position = "plot"
  )

plot_vote_compare

ggsave("model_vote.png", plot_vote_compare, height = 6, width = 10)

### END


