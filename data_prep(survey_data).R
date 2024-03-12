# File is used to clean and prep data for analysis and visuals
source("load_libraries.R")

SURVEY_DATA <- read.csv("data_deploy/Guest Exit Form (Responses) - Form Responses 1.csv")

# STEP 1: Make column names more manageable
colnames(SURVEY_DATA)[1] <- "timestamp"
colnames(SURVEY_DATA)[2] <- "user_id"
colnames(SURVEY_DATA)[3] <- "name"
colnames(SURVEY_DATA)[4] <- "age"
colnames(SURVEY_DATA)[5] <- "gender"
colnames(SURVEY_DATA)[6] <- "phone_model"
colnames(SURVEY_DATA)[7] <- "edu_level"
colnames(SURVEY_DATA)[8] <- "occupation"
colnames(SURVEY_DATA)[9]  <- "shadow_pin_difficulty"
colnames(SURVEY_DATA)[10] <- "rainbow_road_difficulty"
colnames(SURVEY_DATA)[11] <- "headphone_shift_difficulty"
colnames(SURVEY_DATA)[12] <- "hue_hunt_difficulty"
colnames(SURVEY_DATA)[13] <- "math_merkle_difficulty"
colnames(SURVEY_DATA)[14] <- "markov_melodies_difficulty"
colnames(SURVEY_DATA)[15] <- "favorite_games"
colnames(SURVEY_DATA)[16] <- "challenging_parts"
colnames(SURVEY_DATA)[17] <- "Phone_Number"

# STEP 2: Remove bad entries
edu_levels <- 
  c("No Formal Schooling",
    "1st - 5th Standard",
    "6th - 8th Standard",
    "9th - 12th Standard",
    "Some College",
    "Bachelor's Degree",
    "Graduate Degree"
  )

SURVEY_DATA$edu_level <- # adjust value in some cells
  SURVEY_DATA$edu_level %>% 
  map_chr(function(x){
    if(x == "Master's Degree"){return("Graduate Degree")}
    else if(x == "9th-12th Standard"){return("9th - 12th Standard")}
    else {return(x)}
  })

SURVEY_DATA$edu_level <- # fill an 'other' category
  SURVEY_DATA$edu_level %>% 
  map_chr(function(x){
    return(ifelse(x %in% edu_levels,x,NA))
  })

SURVEY_DATA$user_id <- tolower(SURVEY_DATA$user_id) # put all user id's in lower case

# If there are multiple survey submissions for the same user id, keep only the first
duplicates <- SURVEY_DATA %>% 
  group_by(user_id) %>% 
  summarize(count = n()) %>% 
  filter(count > 1)
to_remove <- c()
for(i in 1:nrow(duplicates)){
  a <- which(SURVEY_DATA$user_id == slice(duplicates,i)[['user_id']])
  to_remove <- c(to_remove, a[2:length(a)])
}
SURVEY_DATA <- SURVEY_DATA %>% slice(-1*to_remove)

# STEP 3: curate SURVEY_DATA
# map written likert scale to numbers
SURVEY_DATA$shadow_pin_difficulty <- 
  SURVEY_DATA$shadow_pin_difficulty %>% 
  map_int(function(x){
    if(x == "Very Easy"){v <- 1}
    else if(x == "Easy"){v <- 2}
    else if(x == "Neutral"){v <- 3}
    else if(x == "Hard"){v <- 4}
    else if(x == "Very Hard"){v <- 5}
    else{v <- NA}
  })
SURVEY_DATA$rainbow_road_difficulty <- 
  SURVEY_DATA$rainbow_road_difficulty %>% 
  map_int(function(x){
    if(x == "Very Easy"){v <- 1}
    else if(x == "Easy"){v <- 2}
    else if(x == "Neutral"){v <- 3}
    else if(x == "Hard"){v <- 4}
    else if(x == "Very Hard"){v <- 5}
    else{v <- NA}
  })
SURVEY_DATA$headphone_shift_difficulty <- 
  SURVEY_DATA$headphone_shift_difficulty %>% 
  map_int(function(x){
    if(x == "Very Easy"){v <- 1}
    else if(x == "Easy"){v <- 2}
    else if(x == "Neutral"){v <- 3}
    else if(x == "Hard"){v <- 4}
    else if(x == "Very Hard"){v <- 5}
    else{v <- NA}
  })
SURVEY_DATA$hue_hunt_difficulty <- 
  SURVEY_DATA$hue_hunt_difficulty %>% 
  map_int(function(x){
    if(x == "Very Easy"){v <- 1}
    else if(x == "Easy"){v <- 2}
    else if(x == "Neutral"){v <- 3}
    else if(x == "Hard"){v <- 4}
    else if(x == "Very Hard"){v <- 5}
    else{v <- NA}
  })
SURVEY_DATA$math_merkle_difficulty <- 
  SURVEY_DATA$math_merkle_difficulty %>% 
  map_int(function(x){
    if(x == "Very Easy"){v <- 1}
    else if(x == "Easy"){v <- 2}
    else if(x == "Neutral"){v <- 3}
    else if(x == "Hard"){v <- 4}
    else if(x == "Very Hard"){v <- 5}
    else{v <- NA}
  })
SURVEY_DATA$markov_melodies_difficulty <- 
  SURVEY_DATA$markov_melodies_difficulty %>% 
  map_int(function(x){
    if(x == "Very Easy"){v <- 1}
    else if(x == "Easy"){v <- 2}
    else if(x == "Neutral"){v <- 3}
    else if(x == "Hard"){v <- 4}
    else if(x == "Very Hard"){v <- 5}
    else{v <- NA}
  })


# STEP 4: Make data types appropriate
SURVEY_DATA$timestamp <- SURVEY_DATA$timestamp %>% mdy_hm(tz="Asia/Calcutta")
SURVEY_DATA$age <- as.numeric(SURVEY_DATA$age)
SURVEY_DATA$gender <- factor(SURVEY_DATA$gender,levels = c("Female","Male","Other"))
SURVEY_DATA$edu_level <-
  factor( 
    SURVEY_DATA$edu_level,
    levels= edu_levels,
    ordered = T
  )
SURVEY_DATA$shadow_pin_difficulty <- as.numeric(SURVEY_DATA$shadow_pin_difficulty)
SURVEY_DATA$rainbow_road_difficulty <- as.numeric(SURVEY_DATA$rainbow_road_difficulty)
SURVEY_DATA$headphone_shift_difficulty <- as.numeric(SURVEY_DATA$headphone_shift_difficulty)
SURVEY_DATA$hue_hunt_difficulty <- as.numeric(SURVEY_DATA$hue_hunt_difficulty)
SURVEY_DATA$math_merkle_difficulty <- as.numeric(SURVEY_DATA$math_merkle_difficulty)
SURVEY_DATA$markov_melodies_difficulty <- as.numeric(SURVEY_DATA$markov_melodies_difficulty)
