source("./data_prep(survey_data).R")
source("./data_prep(game_data).R")

# call in tables curated in above files and give them friendlier names
survey <- SURVEY_DATA
i <- INSTANCES
u <- USERS
s <- STATS %>% left_join(SURVEY_DATA, by = 'user_id')

# Histograms
age_bar <-
survey %>% ggplot(aes(x=age)) + 
  geom_histogram() +
  labs(x = "Age",y="Count",title = "Age Distribution")
gender_bar <-
survey %>% filter(!is.na(gender)) %>%  ggplot(aes(x=gender)) + 
  geom_bar() + 
  labs(x="",y="Count",title = "Gender Distribution") +
  geom_text(stat = "count",aes(label=..count..),vjust = -0.2)
edu_bar <-
survey %>% filter(!is.na(edu_level)) %>% ggplot(aes(x=edu_level)) +
  geom_bar() +
  labs(x="",y="Count",title="Education Distribution") +
  geom_text(stat = "count",aes(label=..count..),vjust = -0.2) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1.0, hjust=1))

# Game Difficulty Diagrams
survey %>% ggplot(aes(x=shadow_pin_difficulty)) +
  geom_bar(stat = 'count') +
  labs(x ="Difficulty",y="Count",title="Shadow Pin Difficulty") +
  geom_vline(xintercept = mean(s$shadow_pin_difficulty,na.rm=T),color = 'red') +
  geom_text(stat="count",aes(label=..count..), vjust=-0.2)

survey %>% ggplot(aes(x=rainbow_road_difficulty)) +
  geom_bar(stat = 'count') +
  labs(x ="Difficulty",y="Count",title="Rainbow Road Difficulty") +
  geom_vline(xintercept = mean(s$rainbow_road_difficulty,na.rm=T),color = 'red') +
  geom_text(stat="count",aes(label=..count..), vjust=-0.2)

survey %>% ggplot(aes(x=headphone_shift_difficulty)) +
  geom_bar(stat = 'count') +
  labs(x ="Difficulty",y="Count",title="Headphone Shift Difficulty") +
  geom_vline(xintercept = mean(s$headphone_shift_difficulty,na.rm=T),color = 'red') +
  geom_text(stat="count",aes(label=..count..), vjust=-0.2)

survey %>% ggplot(aes(x=hue_hunt_difficulty)) +
  geom_bar(stat = 'count') +
  labs(x ="Difficulty",y="Count",title="Hue Hunt Difficulty") +
  geom_vline(xintercept = mean(s$hue_hunt_difficulty,na.rm=T),color = 'red') +
  geom_text(stat="count",aes(label=..count..), vjust=-0.2)

survey %>% ggplot(aes(x=math_merkle_difficulty)) +
  geom_bar(stat = 'count') +
  labs(x ="Difficulty",y="Count",title="Mathematical Merkle Difficulty") +
  geom_vline(xintercept = mean(s$math_merkle_difficulty,na.rm=T),color = 'red') +
  geom_text(stat="count",aes(label=..count..), vjust=-0.2)

survey %>% ggplot(aes(x=markov_melodies_difficulty)) +
  geom_bar(stat = 'count') +
  labs(x ="Difficulty",y="Count",title="Markov Melodies Difficulty") +
  geom_vline(xintercept = mean(s$markov_melodies_difficulty,na.rm=T),color = 'red') +
  geom_text(stat="count",aes(label=..count..), vjust=-0.2)

# Generate Models of subjective game difficulty based on demographic factors
m.shadow <- lm(shadow_pin_difficulty~age+gender+edu_level,s)
m.rainbow <- lm(rainbow_road_difficulty~age+gender+edu_level,data = s)
m.headphone <- lm(headphone_shift_difficulty~age+gender+edu_level,data = s)
m.hue <- lm(hue_hunt_difficulty~age+gender+edu_level,data = s)
m.math <- lm(math_merkle_difficulty~age+gender+edu_level,data = s)
m.markov <- lm(markov_melodies_difficulty~age+gender+edu_level,data = s)

# Game against Age
survey %>% pivot_longer(
      cols = c(shadow_pin_difficulty,
           rainbow_road_difficulty,
           headphone_shift_difficulty,
           hue_hunt_difficulty,
           math_merkle_difficulty,
           markov_melodies_difficulty
      ), 
      names_to = "game",
      values_to = "difficulty"
      ) %>% 
ggplot(aes(x=age,y=difficulty)) +
  geom_smooth(aes(color=game),method='lm',se=F)

# Game against Gender
survey %>% pivot_longer(
      cols = c(shadow_pin_difficulty,
           rainbow_road_difficulty,
           headphone_shift_difficulty,
           hue_hunt_difficulty,
           math_merkle_difficulty,
           markov_melodies_difficulty
        ), 
        names_to = c("game","diff"),
        names_sep = ".difficulty" ,
        values_to = "difficulty"
        ) %>% 
ggplot(aes(x=game,y=difficulty)) +
  geom_boxplot(aes(color=gender)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1))

# boxplots
survey %>%
  ggplot()+
    geom_boxplot(aes(x=game_name, y = success_rate)) +
    theme(axis.text.x = element_text(angle = 20, vjust = 1.0, hjust=1))

survey %>%
  ggplot()+
  geom_boxplot(aes(x=game_name, y = success_rate)) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1.0, hjust=1))



# Correlation between subjective measures and quantitative measures
s %>%
  filter(game_name == 'shadow_pin') %>% 
  ggplot(aes(x = failure_rate,y=shadow_pin_difficulty)) + 
    geom_jitter() +
    geom_smooth(method='lm')+
    labs(x="Failure Rate",y="Perceived Difficulty",title = "Shadow Pin Difficulty")


