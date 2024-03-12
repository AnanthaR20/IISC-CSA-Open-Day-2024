source("load_libraries.R")

# Load sqlite data
conn <- dbConnect(RSQLite::SQLite(), "data_deploy/data_post_open_day.db")
INSTANCES <- dbGetQuery(conn, "SELECT * FROM instances WHERE (user_id <> 'lrs' AND user_id <> 'tut')")
USERS <- dbGetQuery(conn,"SELECT * FROM users WHERE (uid <> 'lrs' AND uid <> 'tut')")
dbDisconnect(conn = conn)
USERS <- USERS %>% filter(uid %in% INSTANCES$user_id) # remove all unused user id's

# STEP 1: Make column names more manageable
colnames(USERS)[1] <- "user_id"
colnames(USERS)[2] <- "actual_pin"
colnames(USERS)[5] <- "regular_pin_entry_succ_count"
colnames(USERS)[6] <- "hue_hunt_succ_count"
colnames(USERS)[7] <- "headphone_shift_succ_count"
colnames(USERS)[8] <- "math_merkle_succ_count"
colnames(USERS)[9]  <- "rainbow_road_succ_count"
colnames(USERS)[10] <- "shadow_pin_succ_count"
colnames(USERS)[11] <- "markov_melodies_succ_count"

# STEP 2: Make data types appropriate
USERS$creation_time <- USERS$creation_time %>% ymd_hms(tz="Asia/Calcutta")
INSTANCES$start_time <- INSTANCES$start_time %>% ymd_hms(tz="Asia/Calcutta")
INSTANCES$finish_time <- INSTANCES$finish_time %>% ymd_hms(tz="Asia/Calcutta")

# STEP 3: Remove bad entries
# remove numeric user id entries
INSTANCES <- INSTANCES %>% filter(user_id %in% USERS$user_id)
# remove weird error with some games appending a "-1" multiple times to the end of the result pin
INSTANCES$result_pin <- gsub("-1","",INSTANCES$result_pin)

# STEP 4: curate data
# add actual_pin to INSTANCES for calculation
INSTANCES$actual_pin <- 
  map_chr(
    INSTANCES$user_id, function(x){
      return(USERS$actual_pin[USERS$user_id == x])
    }
  )

# add useful variables to the INSTANCES table
INSTANCES <- INSTANCES %>% 
  mutate(
    game_name = 
      map_chr(game_id,function(x){
        if(x == "RPE"){return("regular_pin_entry")}
        else if(x == "R"){return("shadow_pin")}
        else if(x == "MMM"){return("hue_hunt")}
        else if(x == "IMMM"){return("rainbow_road")}
        else if(x == "MM"){return("markov_melodies")}
        else if(x == "HS"){return("headphone_shift")}
        else if(x == "NM") {return("math_merkle")}
        else {return("improper game_id provided in INSTANCES table")}
      }),
    outcome = 
      map2_chr(result_pin,actual_pin,function(x,y){
        if(is.na(x)){return("abort")}
        else if(x == y){return("success")}
        else{return("failure")}
      }),
    success = ifelse(outcome == "success", 1,0),
    failure = ifelse(outcome == "failure",1,0),
    abort = ifelse(outcome == "abort",1,0),
    digit_score =
      map2_int(result_pin,actual_pin,function(x,y){
        return(4 - pin_diffs(x,y))
      }),
    duration = as.numeric(finish_time - start_time),
    successful_duration = ifelse(success > 0,duration,NA)
  )

# refactor game name so it appears as categorical variable with regular pin entry as reference
INSTANCES$game_name <- 
  factor(INSTANCES$game_name,
         levels = c("regular_pin_entry",
                    "shadow_pin",
                    "rainbow_road",
                    "headphone_shift",
                    "hue_hunt",
                    "math_merkle",
                    "markov_melodies"
                    )
  )


# add useful variables to the USERS table
STATS <- 
INSTANCES %>% 
  group_by(user_id,game_name) %>% 
  summarize(
    instance_count = n(),
    success_rate = round(mean(success),2),
    failure_rate = round(mean(failure),2),
    abort_rate = round(mean(abort),2),
    attempts = length(iid[outcome != 'abort']),
    average_digit_score = 
      ifelse(length(iid[outcome != 'abort']) < 1, # handle case where instance count is zero after removing NA
        NA,
        ifelse(game_name == 'markov_melodies',NA,round(mean(digit_score,na.rm = T),2))
      ),
    average_duration = 
      ifelse(length(iid[outcome != 'abort']) < 1,
        NA,
        round(mean(duration,na.rm = T),2)
      ),
    quickest_success =
      ifelse(length(iid[outcome == 'success']) < 1,
        NA,
        round(min(successful_duration,na.rm = T),2)
      ),
    # outcome delta
    outcome_first_attempt = 
      ifelse(length(iid[outcome != 'abort']) < 1,
        NA,
        outcome[which(finish_time == min(finish_time,na.rm = T))]
      ),
    outcome_last_attempt =
      ifelse(length(iid[outcome != 'abort']) < 1,
        NA,
        outcome[which(finish_time == max(finish_time,na.rm = T))]
      ),
    # error delta
    digit_score_first_attempt = 
      ifelse(length(iid[outcome != 'abort']) < 1,
        digit_score[which(iid == min(iid))],
        ifelse(game_name == 'markov_melodies',NA,digit_score[which(finish_time == min(finish_time,na.rm = T))])
      ),
    digit_score_last_attempt =
    ifelse(length(iid[outcome != 'abort']) < 1,
      digit_score[which(iid == max(iid))],
      ifelse(game_name == 'markov_melodies',NA,digit_score[which(finish_time == max(finish_time,na.rm = T))])
    ),
    # speed delta
    duration_first_attempt = 
      ifelse( length(iid[outcome != 'abort']) < 1,
        duration[which(iid == min(iid))],
        duration[which(finish_time == min(finish_time,na.rm = T))]
      ),
    duration_last_attempt = 
      ifelse( length(iid[outcome != 'abort']) < 1,
        duration[which(iid == max(iid))],
        duration[which(finish_time == max(finish_time,na.rm = T))]
      ),
  )

STATS <- 
  STATS %>% mutate(
    outcome_delta = str_c(outcome_first_attempt, " -> ",outcome_last_attempt),
    digit_score_delta = digit_score_last_attempt - digit_score_first_attempt,
    duration_delta = duration_first_attempt - duration_last_attempt
  )

STATS$outcome_delta <- as.factor(STATS$outcome_delta)

