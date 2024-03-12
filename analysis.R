source("./data_prep(survey_data).R")
source("./data_prep(game_data).R")

# call in tables curated in above files and give them friendlier names
survey <- SURVEY_DATA
i <- INSTANCES
u <- USERS
s <- STATS %>% left_join(SURVEY_DATA, by = 'user_id')

instances_summary_table <-
  i %>%
    group_by(game_name) %>% 
    summarize(
      total_instances = n(),
      success_rate = round(mean(success),2),
      failure_rate = round(mean(failure),2),
      abort_rate = round(mean(abort),2),
      average_entry_time = round(mean(duration,na.rm = T),2)
    )
instances_summary_table <- instances_summary_table[order(instances_summary_table$total_instances, decreasing=TRUE),]


users_summary_table <-
  i %>% 
    group_by(game_name,user_id) %>%
    summarize(
      user_success_rate = mean(success),
      user_failure_rate = mean(failure),
      user_abort_rate = mean(abort),
      user_entry_time = mean(duration,na.rm = T),
    ) %>% 
    group_by(game_name) %>% 
    summarize(
      total_users = n_distinct(user_id),
      typical_success_rate = round(mean(user_success_rate),2),
      typical_failure_rate = round(mean(user_failure_rate),2),
      typical_abort_rate = round(mean(user_abort_rate),2),
      typical_entry_time = round(mean(user_entry_time,na.rm = T),2)
    )
users_summary_table <- users_summary_table[order(users_summary_table$total_users,decreasing = TRUE),]


survey_summary_table <-
  survey %>% 
     pivot_longer(
       cols = colnames(survey)[grep("_difficulty",colnames(survey))],
       names_to = c("game_name","diff"),names_sep ="_difficulty",
       values_to = "difficulty") %>% 
     mutate(
       rated = ifelse(is.na(difficulty),0,1),
       gender_binary = ifelse(gender == "Female",0,1),
     ) %>% 
     group_by(game_name) %>% 
     summarize(
       users_who_rated=sum(rated),
       ave_age = round(mean(age[which(rated == 1)],na.rm=T),1),
       difficulty= round(mean(difficulty[which(rated == 1)],na.rm=T),2),
       proportion_female = round(mean(gender_binary[which(rated == 1)],na.rm = T),2)
     )
survey_summary_table <- survey_summary_table[order(survey_summary_table$users_who_rated,decreasing = TRUE),]


# create a model for each variable to examine the significance of the effect of the game variable
success_rate_model <- lm(success_rate~game_name,data = s)
failure_rate_model <- lm(failure_rate~game_name,data = s)
abort_rate_model <- lm(abort_rate~game_name,data = s)
attempts_model <- lm(attempts~game_name,data = s)
digit_score_model <- lm(average_digit_score~game_name,data = s)
digit_score_first_attempt_model <- lm(digit_score_first_attempt~game_name,data = s)
digit_score_last_attempt_model <- lm(digit_score_last_attempt~game_name,data = s)
digit_score_delta_model <- lm(digit_score_delta~game_name,data = s)
duration_model <- lm(average_duration~game_name,data = s)
duration_first_attempt_model <- lm(duration_first_attempt~game_name,data = s)
duration_last_attempt_model <- lm(duration_last_attempt~game_name,data = s)
duration_delta_model <- lm(duration_delta~game_name,data = s)
quickest_success_model <- lm(quickest_success~game_name,data = s)

# create model regressing on each var for demographics
dem_success_rate_model <- lm(success_rate~edu_level+gender+age,data = s)
dem_failure_rate_model <- lm(failure_rate~edu_level+gender+age,data = s)
dem_abort_rate_model <- lm(abort_rate~edu_level+gender+age,data = s)
dem_attempts_model <- lm(attempts~edu_level+gender+age,data = s)
dem_digit_score_model <- lm(average_digit_score~edu_level+gender+age,data = s)
dem_digit_score_first_attempt_model <- lm(digit_score_first_attempt~edu_level+gender+age,data = s)
dem_digit_score_delta_model <- lm(digit_score_delta~edu_level+gender+age,data = s)
dem_duration_model <- lm(average_duration~edu_level+gender+age,data = s)
dem_duration_first_attempt_model <- lm(duration_first_attempt~edu_level+gender+age,data = s)
dem_duration_delta_model <- lm(duration_delta~edu_level+gender+age,data = s)
dem_quickest_success_model <- lm(quickest_success~edu_level+gender+age,data = s)

# Analyze various game orders
# Success Rate, Digit Score First Attempt, Quickest Success
rr <- s %>% filter(game_name == 'rainbow_road')
hh <- s %>% filter(game_name == 'hue_hunt')
hs <- s %>% filter(game_name == 'headphone_shift')
sp <- s %>% filter(game_name == 'shadow_pin')

rr_sp1 <- t.test(rr$success_rate,sp$success_rate)
hh_sp1 <- t.test(hh$success_rate,sp$success_rate)
hs_sp1 <- t.test(hs$success_rate,sp$success_rate)
rr_hh1 <- t.test(rr$success_rate,hh$success_rate)
rr_hs1 <- t.test(rr$success_rate,hs$success_rate)
hh_hs1 <- t.test(hh$success_rate,hs$success_rate)

rr_sp3 <- t.test(rr$digit_score_first_attempt,sp$digit_score_first_attempt)
hh_sp3 <- t.test(hh$digit_score_first_attempt,sp$digit_score_first_attempt)
hs_sp3 <- t.test(hs$digit_score_first_attempt,sp$digit_score_first_attempt)
rr_hh3 <- t.test(rr$digit_score_first_attempt,hh$digit_score_first_attempt)
rr_hs3 <- t.test(rr$digit_score_first_attempt,hs$digit_score_first_attempt)
hh_hs3 <- t.test(hh$digit_score_first_attempt,hs$digit_score_first_attempt)

rr_sp2 <- t.test(rr$quickest_success,sp$quickest_success)
hh_sp2 <- t.test(hh$quickest_success,sp$quickest_success)
hs_sp2 <- t.test(hs$quickest_success,sp$quickest_success)
rr_hh2 <- t.test(rr$quickest_success,hh$quickest_success)
rr_hs2 <- t.test(rr$quickest_success,hs$quickest_success)
hh_hs2 <- t.test(hh$quickest_success,hs$quickest_success)


# Did people improve?
# looking at deltas
s %>% filter(!is.na(outcome_delta),game_name != 'markov_melodies') %>% 
  ggplot(aes(x=digit_score_first_attempt,y = digit_score_last_attempt)) +
    geom_jitter(aes(color = outcome_delta)) +
    facet_wrap(~game_name) +
    geom_abline(a=1,b=0)


rr_improve1 <- t.test(rr$digit_score_delta)
hh_improve1 <- t.test(hh$digit_score_delta)
hs_improve1 <- t.test(hs$digit_score_delta)
sp_improve1 <- t.test(sp$digit_score_delta)

rr_improve2 <- t.test(rr$duration_delta)
hh_improve2 <- t.test(hh$duration_delta)
hs_improve2 <- t.test(hs$duration_delta)
sp_improve2 <- t.test(sp$duration_delta)





