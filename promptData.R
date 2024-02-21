library(haven)
library(dplyr)
library(forcats)
library(stringr)
library(mice)
library(checkmate)
#devtools::install_github("ben-aaron188/rgpt3", force = TRUE)
library(rgpt3)
library(openai)
library(readr)


# Final dataset
# load("GPT3_data_complete_2019.Rdata")

######################################## Here begins the data transformation and prompting and resampling #####################
# Read own created data:
india_new <- read_csv("india_new.csv")
india_new <- india_new[, -1]  

#################################### API-Key Set Up ##########################################################
# api_key <- "own API key"

# Add Api_key
gpt3_authenticate("c:/Users/MM/Documents/access_key.txt")

### If not working then do this:
api_key <- readLines("c:/Users/MM/Documents/access_key.txt")
set_openai_key(api_key)

## If still not working then do this way:
library(httr)
headers <- add_headers("Authorization" = paste("Bearer", api_key)) # Set the API key in the header
response <- GET("https://api.openai.com/v1/engines/davinci/completions", headers = headers) # Test the authentication by making a simple request

# --> API Key is set up!

###################################### First time Prompting (GPT_prompt_completion.Rdata) #########################################################

# Create Prompts-Data Frame
prompts_df <- data.frame(
  prompt_id = india_new$lfdn,
  prompt = india_new$prompt)

example = rgpt(prompt_role_var = rep('user', 1113)
               , prompt_content_var = prompts_df$prompt
               , id_var = prompts_df$prompt_id
               , param_max_tokens = 10
               , param_n = 1
               , param_temperature = 0.9)

save(example, file = "example.Rdata")
completions_df <- example[[1]] # extract completions
metadata_df<- example[[2]] # extract metadata

save(india_new, prompts_df, completions_df, metadata_df, file = "GPT_prompt_completion.Rdata")
load("GPT_prompt_completion.Rdata")
# save input and output
# --> First prompt completion is saved

################################################ Keyword Matching #########################################################################

## Create keywords to search for: 
sequences <- c("BJP", "Bharatiya Janata Party", "Congress", "Left front Parties", "Communist Party", "Third Front", "Regional Party", "Other party", 
               "Left front Parties/Communist Party", "right-wing/conservative", "right-wing", "conservative", "right/conservative", "left/center-left", 
               "left","center-left",
               "not", "invalid", "cannot")

check_sequences <- c("not", "invalid", "cannot")

# This list defines the categories for the parties. If at least two words (one each) of two different categories are in one sentence, 
# that sentence gets flagged for manual checking.

sequences_list <- list(bjp = c("BJP", "Bharatiya Janata Party", "right-wing/conservative", "right-wing", "conservative", "right/conservative"),
                       congress = c("Congress", "left/center-left", "left","center-left"),
                       leftfront = c("Left front Parties", "left","Left front Parties/Communist Party"),
                       communist = c("Communist Party", "Left front Parties/Communist Party"),
                       thirdfront = c("Third Front"),
                       regionalparty = c("Regional Party"),
                       otherparty = c("Other party"))

# Get Matches Vector
get_matches_vector <- function(keywords, column, delimiter = " ", replace_zero_length = TRUE) {
  assert_character(keywords, min.len = 1)
  assert_character(column)
  assert_character(delimiter, any.missing = FALSE, len = 1,)
  assert_logical(replace_zero_length, any.missing = FALSE, len = 1)
  
  keywords <- gsub("([.\\[\\{\\(\\*\\+\\?\\^\\$\\|\\\\])", "\\\\\\1", keywords, perl = TRUE)
  pattern <- paste0("\\b\\w*(", paste(keywords, collapse = "|"), ")\\w*\\b")
  matches <- str_extract_all(column, regex(pattern, ignore_case = TRUE))
  
  if (replace_zero_length) {
    matches <- lapply(matches, function(x) if(identical(x, character(0))) NA_character_ else x)
  }
  matches <- sapply(matches, paste, collapse = delimiter)
  return(matches)
}

get_matches_vector(keywords = sequences, column = completions_df$gpt_content)

# Function get_check_value
get_check_values <- function(check_keywords, matches, split_by = " ") {
  assert_character(check_keywords, min.len = 1)
  assert_character(matches)
  assert_character(split_by, any.missing = FALSE, len = 1)
  
  check_manual_pattern <- paste0("\\b(", paste0(check_keywords, collapse = "|"), ")\\b")
  check_manual <- unlist(lapply(str_split(matches, split_by), function (x) {as.numeric(any(x %in% check_keywords))}))
  
  matches_split <- str_split(matches, pattern = " ")
  
  duplicated_party_vector <- unlist(lapply(lapply(lapply(lapply(matches_split, function(sentence) {
    sapply(sequences_list, function(category) {
      sum(str_count(tolower(sentence), regex(paste0("\\b\\w*(", paste(category, collapse = "|"), ")\\w*\\b"), ignore_case = TRUE)))
    })}), function (x) {x  != 0}), sum), function (x) {x > 1}))
  
  check_label <- ifelse(duplicated_party_vector == 1 | check_manual == 1, 1, 0)
  return(check_label)
}

get_check_values(c("cannot", "not", "invalid"), get_matches_vector(keywords = sequences, column = completions_df$gpt_content))

# Function create_match_completions_df
create_match_completions_df <- function (df, keywords, check_keywords, ...) {
  assert_data_frame(df, col.names = "named", types = c("numeric", "character", "character", "numeric") , ncols = 6)
  assert_character(keywords, min.len = 1)
  assert_character(check_keywords)
  
  matches <- get_matches_vector(keywords = sequences, column = df[["gpt_content"]])
  check_manual <- get_check_values(check_keywords, matches)
  df <- data.frame(
    id = df[["id"]],
    completion = df[["gpt_content"]],
    matched = matches,
    check_manually = check_manual
  )
  return(df)
}

match_completions <- create_match_completions_df(completions_df, keywords = sequences, check_keywords = c("cannot", "invalid", "not"))

################################################## Create mc_india_new & GPT3india_manualcheck.csv #############################################################

# Joining Match completions and GLES datasets by ID to compare match with vote
mc_india <- left_join(match_completions, india_new, by = c("id" = "lfdn"))

# Export for manual checks
write.csv(mc_india, file = "GPT3india_manualcheck.csv", col.names = T, row.names = F)

########################################################### offline checking #######################################################

###################################################### Resampling first time & Storing resampled data ################################################

# Resampling 282 entries
resample <- read.csv("GPT3india_manual_resample.csv") # read in data for resampling
resample <- resample %>% mutate(id_sampleno = paste0(id, sep = "_"))

prompts2_df <- data.frame(
  prompt_id = resample$id_sampleno,
  prompt = resample$prompt)

example2 = rgpt(prompt_role_var = rep('user', 282)
                , prompt_content_var = prompts2_df$prompt
                , id_var = prompts2_df$prompt_id
                , param_max_tokens = 10
                , param_n = 1
                , param_temperature = 0.9)

# Feed prompts
completions_resample_df <- example2[[1]] # extract completions
metadata_resample_df<- example2[[2]] # extract metadata

# Store completions
save(example2, completions_resample_df, metadata_resample_df, file = "GPTindia_prompt_resample.Rdata") # save input and output
load("GPTindia_prompt_resample.Rdata")

# Functions:
get_matches_vector(keywords = sequences, column = completions_resample_df$gpt_content)
get_check_values(c("cannot", "not", "invalid"), get_matches_vector(keywords = sequences, column = completions_resample_df$gpt_content))
match_completions_resample <-  create_match_completions_df(completions_resample_df, keywords = sequences, check_keywords = c("cannot", "not", "invalid"))

write.csv(match_completions_resample, file = "GPT3india_manualchecks_resample.csv")

#################################################### offline checking ###############################################################################

#################################################### Creating data set to resample Second Time  ##########################################################################

# Import checked resampled data and build new identifier for merging
match_completions_resample <- read.csv("GPT3india_manual_resample_checked.csv")

match_completions_resample <- match_completions_resample %>%
    arrange(X) %>%  
    mutate(id_sampleno = paste(id, sep = "_")) %>%
    select(-X)

resample <- resample %>% arrange(id)

##################################################### Identifier ############################################################

# Add new identifier and  Check if identifier is working
is_identifier <- function(column) {length(unique(column)) == length(column)}

is_key_unique <- is_identifier(resample$id_sampleno) # check for resample
if (is_key_unique) {
  print("Key column is unique.")
} else {
  warning("Key column is not unique.")
}

is_key_unique <- is_identifier(match_completions_resample$id_sampleno) # check for matching data
if (is_key_unique) {
  print("Key column is unique.")
} else {
  warning("Key column is not unique.")
}

### Key column is unique!

############################################ Joining Resampled data and GESIS data ###################################################
### Create mc_resample

# Joins matching and GESIS datasets by ID to compare match with vote

mc_resample <- left_join(match_completions_resample, resample, by = c("id_sampleno" = "id_sampleno"))
all(mc_resample$id.x == mc_resample$id.y) # Check if ID columns are the same: FALSE

mc_resample <- mc_resample %>%
  select(-id.y) %>% # remove duplicate ID
  rename(id = id.x) %>% # rename ID variable
  mutate(resampled = 1) # indicator for resampled observations for merging

save(mc_resample, file = "mc_resample.Rdata") # save as backup
load("mc_resample.Rdata")
write.csv(mc_resample, file = "GPT3india_manualchecks_resample_merged.csv")

################################################### Offline checking #############################################################

############################################# Finally resampling second time #####################################################
# Import data: n = 154
resample_2 <- read.csv("GPT3India_manualchecks_resample_merged.csv") 

prompts3_df <- data.frame(
  prompt_id = resample_2$id_sampleno,
  prompt = resample_2$prompt)

example3 = rgpt(prompt_role_var = rep('user', 154)
                , prompt_content_var = prompts3_df$prompt
                , id_var = prompts3_df$prompt_id
                , param_max_tokens = 10
                , param_n = 1
                , param_temperature = 0.9)


completions_resample_2_df <- example3[[1]] # extract completions
metadata_resample_2_df<- example3[[2]] # extract metadata

save(resample_2, completions_resample_2_df, metadata_resample_2_df, file = "GPT_prompt_resample_2.Rdata")
load("GPT_prompt_resample_2.Rdata")

# Match 1: Completions - Party Keywords
get_matches_vector(keywords = sequences, column = completions_resample_2_df$gpt_content)
get_check_values(c("cannot", "not", "invalid"), get_matches_vector(keywords = sequences, column = completions_resample_2_df$gpt_content))
match_completions_resample_2 <- create_match_completions_df(completions_resample_2_df, keywords = sequences, check_keywords = c("cannot", "not", "invalid"))

# Create mc_resample_2:
# Joining second resampled data and GESIS datasets by ID to compare match with vote
mc_resample_2 <- left_join(match_completions_resample_2, resample_2, by = c("id" = "id"))
save(mc_resample_2, file = "mc_resample_2.Rdata") # save as backup
load("mc_resample_2.Rdata")
write.csv(mc_resample_2, file = "GPT3india_manualchecks_resample_2.csv")

########################################################### Offline checking ##################################################################################

################################################ Read the manually (second) resampled checked data ###########################################################

mc_resample_2_checked <- read.csv("GPT3india_manual_resample_2_checked.csv")

## Merging original and re-sampled datasets
# First completion
mc_india <- read.csv("GPT3india_manual_checked.csv")

# mc_resample should be in environment from resampling script, otherwise saved as mc_resample.Rdata
mc_india <- mc_india %>% 
  mutate(id_sampleno = paste0(id, sep = "_")) %>% 
  mutate(id = paste0(id, sep = "_")) 

# Second completion
mc_resample_1_checked <- read.csv("GPT3india_manualchecks_resample_merged.csv")
mc_resample_1_checked <- mc_resample_1_checked %>%
  mutate(completion_2 = NA, matched_2 = NA, check_manually_2 = NA, manipulated_2 = NA, matched_new_2 = NA, resampled_2 = NA)

# Third completion see above (mc_resample_2_checked)
### Final data:
firstsample <- mc_india
secondsample <- read.csv("GPT3india_manual_resample_checked_all.csv")
first_second <- left_join(secondsample, firstsample, by = c("id" = "id")) ## Join First and Second Samples
colnames(first_second) <- gsub("\\.y", "", colnames(first_second))
colnames(first_second) <- gsub("\\.x", ".1", colnames(first_second))

# Merging first data frame which has been prompted for the first time with joined dataframe of first and second prompted data frame:
x <- merge(firstsample, first_second, all =TRUE)
x$id <- as.numeric(gsub("_", "", x$id))
x_sorted <- x[order(x$id), ]

# Renaming the third sampling data set: the e.g. "completion.2" means it was reprompted for the second time.
thirdsample <- read.csv("GPT3india_manual_resample_2_checked.csv")
colnames(thirdsample)[colnames(thirdsample) == "completion"] <- "completion.2"
colnames(thirdsample)[colnames(thirdsample) == "matched"] <- "matched.2"
colnames(thirdsample)[colnames(thirdsample) == "check_manually"] <- "check_manually.2"
colnames(thirdsample) <- gsub("\\.y", "", colnames(thirdsample))
colnames(thirdsample) <- gsub("\\.x", ".1", colnames(thirdsample))
thirdsample$id <- as.numeric(gsub("_", "", thirdsample$id))
thirdsample <- thirdsample[order(thirdsample$id), ]

## Final Data set has duplicated rows
india_final <- merge(x_sorted, thirdsample, all = TRUE)
write.csv(india_final, file = "GPT3india_finalmanual.csv")

######################################################## Offline check (deleting duplicate rows) ############################################

mc_india_final <- read.csv("GPT3india_final.csv")

######################################################## Checking if Vote outcomes of GPT match with original Votes ##########################
#BJP
check_match_bjp <- function (mc_india_final) {
  pattern_bjp <- paste0(".*", paste(c("BJP","Bharatiya Janata Party", "right-wing/conservative", "right-wing", "conservative", "right/conservative"), collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_bjp, ignore.case = TRUE)
}
# Congress
check_match_congress <- function (mc_india_final) {
  pattern_congress <- paste0(".*", paste(c("Congress", "left/center-left", "left","center-left"), collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_congress, ignore.case = TRUE)
}
# Left Front
check_match_leftfront <- function (mc_india_final) {
  pattern_leftfront <- paste0(".*", paste(c("Left front Parties", "left","Left front Parties/Communist Party"), collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_leftfront, ignore.case = TRUE)
}
# Communist 
check_match_communist<- function (mc_india_final) {
  pattern_communist <- paste0(".*", paste(c("Communist Party", "Left front Parties/Communist Party"), collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_communist, ignore.case = TRUE)
}
# Third Front
check_match_thirdfront<- function (mc_india_final) {
  pattern_thirdfront<- paste0(".*", paste(c("Third Front"), collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_thirdfront, ignore.case = TRUE)
}
# Regional Party
check_match_regionalparty <- function (mc_india_final) {
  pattern_regionalparty <- paste0(".*", paste(c("Regional Party", "regional"), collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_regionalparty, ignore.case = TRUE)
}

# Other party
check_match_otherparty <- function (mc_india_final) {
  pattern_otherparty <- paste0(".*", paste(c("Other Party", "other"), collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_otherparty, ignore.case = TRUE)
}

# not
check_match_not <- function (mc_india_final) {
  pattern_not <- paste0(".*", paste("not", collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_not, ignore.case = TRUE)
}

# cannot gewählt
check_match_cannot <- function (mc_india_final) {
  pattern_cannot <- paste0(".*", paste("cannot", collapse = "|"), ".*")
  lapply(mc_india_final$matched_new, grepl, pattern = pattern_cannot, ignore.case = TRUE)
}

match_vote_outcome <-  case_when(
  mc_india_final$vote %in% "BJP" ~ check_match_bjp(mc_india_final),
  mc_india_final$vote %in% "Congress" ~ check_match_congress(mc_india_final),
  mc_india_final$vote %in% "Left Front" ~ check_match_leftfront(mc_india_final),
  mc_india_final$vote %in% "Communist" ~ check_match_communist(mc_india_final),
  mc_india_final$vote %in% "Third Front" ~ check_match_thirdfront(mc_india_final),
  mc_india_final$vote %in% "Regional Party" ~ check_match_regionalparty(mc_india_final),
  mc_india_final$vote %in% "Other Party" ~ check_match_otherparty(mc_india_final),
  mc_india_final$vote %in% "not voted" ~ check_match_not(mc_india_final),
  mc_india_final$vote %in% "cannot vote" ~ check_match_cannot(mc_india_final)
)
match_vote_outcome <- lapply(match_vote_outcome, function(x) {
  x[is.null(x)] <- NA
  x
})

## New Column added: match_vote_outcome (TRUE OR FALSE)

mc_india_final <- mc_india_final %>% 
  mutate(match_vote_outcome = unlist(match_vote_outcome))

#########################################################################################################################################

# Define a function to check if any of the parties are present in the columns
check_parties <- function(row) {
  any(row %in% c("BJP", "Congress","regional party", "Regional Party", "Left" , "Third Front", "Other party", "Communist Party", "Left Communist Party", "Left Front",
                 "Left Front Parties"  , "Left front Parties" ,"Left front" , "Left front Parties Communist Party"))
}

# Apply the function row-wise across the specified columns
mc_india_final$match_vote_outcome <- apply(mc_india_final[, c("matched", "matched.1", "matched.2")], 1, check_parties)

############################################ Create a new column calles vote_gpt which has all the votes of GPT #####################################

get_vote <- function(row) {
  if ("BJP" %in% row) {
    return("BJP")
  } else if ("Congress" %in% row) {
    return("Congress")
  } else if ("RegionalParty" %in% row | "regional party" %in% row) {
    return("Regional Party")
  } else if ("Left" %in% row | "Communist Party" %in% row | "Left Communist Party" %in% row |
             "Left Front" %in% row | "Left Front Parties" %in% row | "Left front" %in% row |
             "Left front Parties Communist Party" %in% row) {
    return("Left Front/Communist Party")
  } else if ("Third Front" %in% row) {
    return("Third Front")
  } else if ("Other party" %in% row) {
    return("Other Party")
  } else if ("not" %in% row | "cannot" %in% row) {
    return("No vote")
  } else {
    return("Unknown")
  }
}
mc_india_final$vote_gpt <- apply(mc_india_final[, c("matched", "matched.1", "matched.2")], 1, get_vote)
save(mc_india_final, file = "GPT3_data_complete_2019.Rdata") # save final dataset
load("GPT3_data_complete_2019.Rdata")

## END