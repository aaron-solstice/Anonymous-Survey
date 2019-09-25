library(tidyverse)
library(googlesheets)
library(splitstackshape)
library(magrittr)
library(lubridate)
library(ggthemes)
library(shinydashboard)
library(shiny)
library(plotly)

put_last_column_in_front <- function(d){
  return(d[,c(length(d), 1:(length(d) - 1)  )]) 
}


make_data_long <- function(data){
  data_long <- data %>% 
    gather(question, answer, -Timestamp) %>% 
    separate(question, into = c("q_num", "question_text"), sep = "[^[:alnum:]+]", extra = "merge", convert = T) %>% 
    mutate(question_text = trimws(question_text),
           q_num = trimws(q_num),
           datestamp = as.Date(Timestamp,format = "%m/%d/%Y %H:%M:%S") )
  
  return(data_long)
}

analyze_dataset <- function(data) {

data_long <- make_data_long(data)

max_date <- max(data_long$datestamp)

data_long %<>% select(-datestamp)

#n who completed the survey
n_respondents <- data %>% 
  summarize(n_completes = n()) 


#the above can be added once we create a list of all the dataframes.

five_pt_scale_data <- data_long %>% 
  filter(q_num %in% as.character(c(1:38, 40:83)) ) 

##computations

#avg by respondent to produce a histogram, or series of histograms.
avg_respondent_5_pt <- five_pt_scale_data %>% 
  group_by(Timestamp) %>% 
  summarize(avg_answer = mean(as.numeric(answer)) )

#avg score by question, rank the scores, classify them, and get their SD dispersion
#if(<=1.6,"Poor",<=2.6,"Fair",<=3.6,"Good",4.6,"Very Good","Excellent"))))
#c("Poor", "Fair", "Good", "Very Good", "Excellent")
avg_question_score <- five_pt_scale_data %>% 
  group_by(q_num, question_text) %>% 
  summarize(avg_answer = mean(as.numeric(answer)), stdev_answer = sd(as.numeric(answer)) ) %>% 
  ungroup() %>% 
  mutate(rk = rank(-avg_answer),
         p_rk_sd = percent_rank(stdev_answer),
         rk_sd = rank(stdev_answer),
         category = cut(avg_answer, breaks = c(1, 1.6, 2.6, 3.6, 4.6, 5), labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")) )

#the percentages of scores by question (1s, 2s, 3s, to 5s, etc.)
q_counts_percents <- data_long %>% 
  count(q_num, question_text, answer) %>% 
  group_by(q_num, question_text) %>% 
  mutate(p = n/sum(n) ) %>% 
  arrange(q_num)

q_counts_percents_wide <- q_counts_percents %>% 
  select(-n) %>% 
  mutate(answer= as.numeric(answer)) %>% 
  filter(!is.na(answer)) %>% 
  spread(answer, p)
  
#aggregate vars

agg_vars_qnum <- read_tsv("aggregate vars.tsv", col_names = F) %>% 
  cSplit(., 'X1', '$') %>% 
  gather(q_var, t, -X1_01) %>% 
  separate(t, into = c("q_num", "question_text"), sep = "[^[:alnum:]+]", extra = "merge", convert = T) 

agg_vars <- agg_vars_qnum %>% 
  filter(complete.cases(.),  !grepl("a", q_num), !grepl("b", q_num)) %>% 
  select(Grouping = 1, q_num) %>% 
  mutate(Grouping = as.character(Grouping),
         q_num = trimws(q_num))

agg_vars_qnum_map <- agg_vars_qnum %>% filter(!is.na(q_num)) %>% select(-q_var) %>% rename(Grouping = X1_01)


#summarize the responses by group:
group_level_summary <- avg_question_score %>% 
  left_join(agg_vars) %>% 
  filter(!is.na(Grouping) ) %>%
  group_by(Grouping) %>% 
  summarize(aggregate_average_score = mean(avg_answer), total_average_score = sum(avg_answer), rk_dispersion = mean(rk_sd) )


#summarize the answer counts by group
answer_counts_by_group <- q_counts_percents %>% 
  left_join(agg_vars) %>% 
  filter(!is.na(Grouping)) %>% 
  ungroup() %>% 
  group_by(Grouping, answer) %>% 
  summarize(n = sum(n)) %>% 
  spread(answer, n, fill = 0) 
  
#count answers overall.
overall_answer_percents <- five_pt_scale_data %>% 
  count(answer) %>% 
  mutate(p = n/sum(n))

top2box <- overall_answer_percents %>% 
  filter(answer > 3) %>% 
  summarize(top2box = sum(p))

bottom2box <- overall_answer_percents %>% 
  filter(answer < 3) %>% 
  summarize(bottom2box = sum(p))

#ENPS Analysis
q87 <- data_long %>% 
  filter(q_num ==  87) %>% 
  count(answer) %>% 
  filter(!is.na(answer)) %>% 
  bind_rows(data.frame(answer = as.character(c(1:10)[!c(1:10) %in% .$answer]) ) ) %>%  
  mutate(n = ifelse(is.na(n),0, n )) %>% 
  mutate(p = n/sum(n)) %>% 
  arrange(as.numeric(answer) )


enps <- q87 %>% 
  select(-n) %>% 
  spread(answer, p) %>% 
  mutate(enps_9_10 = `9` + `10`, enps_1_6 = `1` + `2` + `3` + `4` + `5` + `6`,
         enps = enps_9_10 - enps_1_6) %>% 
  gather(measure, value) %>% 
  mutate(measure = factor(measure, ordered = T, levels = c(1:10, "enps_9_10", "enps_1_6", "enps"), labels = c(1:10, "9 + 10", "1 thru 6", "ENPS"))) %>% 
  arrange(measure)


##open ends
res <- try({
  
  coded_oes <- data_long %>% 
  filter(grepl("coded", question_text)) 

max_responses <- coded_oes %>% 
  mutate(item_count = stringr::str_count(answer, ",") + 1) %>% 
  .$item_count %>% max(na.rm = T)

new_vars <- paste0("a", 1:max_responses)

coded_oes_long <- coded_oes %>% 
  separate(answer, into = new_vars, sep = ",") %>% 
  gather(new_vars, key = "n", value = "answer") %>% 
  mutate(answer= trimws(answer))

summary_oe_data <- coded_oes_long %>% 
  filter(!is.na(answer)) %>% 
  count(q_num, question_text, answer) %>% 
  group_by(q_num, question_text) %>% 
  mutate(p = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(question_text = str_replace(question_text, "coded. ", "")) })

if(class(res) == "try-error"){browser()}
##end of open end analysis


#create series of data frames w/ the wave_id added at the end.
data_list <-  list(data_long, summary_oe_data, answer_counts_by_group,   group_level_summary, n_respondents, five_pt_scale_data, avg_respondent_5_pt, avg_question_score, q_counts_percents, q_counts_percents_wide, overall_answer_percents, top2box, bottom2box, q87, enps)
names(data_list) <-   c("data_long", "summary_oe_data", "answer_counts_by_group", "group_level_summary", "n_respondents", "five_pt_scale_data", "avg_respondent_5_pt", "avg_question_score", "q_counts_percents", "q_counts_percents_wide", "overall_answer_percents", "top2box", "bottom2box", "q87", "enps")

data_list %<>% 
  map(~mutate(., wave_date = max_date )) %>% 
  map(~ungroup(.)) %>% 
  map(~put_last_column_in_front(.))

x <- data_list %>% 
  map(~nest(., -wave_date)) %>% 
  bind_rows(.) 

y <- add_column(table_name = names(data_list), x)

return(y)
}


wave_tibbles <- function(d){
  
  li <-  c(names(d)[!names(d) %in% "table_name"] )
  return(li )
}

