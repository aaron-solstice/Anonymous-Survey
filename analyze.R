source("functions and libraries.R")




#take a list variable where first value is wb and second is the ws
read_google_sheet <-  function(wb_ws_li){

  
  googlesheets::gs_read_csv(googlesheets::gs_title(wb_ws_li$wb), ws = wb_ws_li$ws) %>% return()
}


wb_ws_lists <- list(list(wb = "Copy of Solstice Anonymous Review (Responses)", ws = "Copy of Form Responses 1"),
           list(wb = "Solstice Anonymous Survey", ws = "Sheet1")

           )

wave_li <- lapply(wb_ws_lists, function(x) read_google_sheet(x))

# wave_df_oe <- googlesheets::gs_read(ss=gs_t <- googlesheets::gs_title("Copy of Solstice Anonymous Review (Responses)"), ws = "OE scoring template") %>% 
#   select(c(Timestamp, contains("coded")) )


oe_li <- list(list(wb = "Copy of Solstice Anonymous Review (Responses)", ws = "OE scoring template expanded"),
              list(wb = "Solstice Anonymous Survey", ws = "Sheet2") )

wave_li_oe <- lapply(oe_li, function(x) read_google_sheet(x)) %>% bind_rows() %>% select(Timestamp, contains("coded"))


wave_li <- wave_li %>% 
  map(~left_join(., wave_li_oe))

# wave_df <- wave_df %>% left_join(wave_df_oe)

create_fake_wave <- function(a_wave, fake_waves){
  
if(fake_waves >0 ){
fake_wave <-   a_wave %>% 
    sample_n(size = 100, replace = T) %>% 
    rowwise() %>% 
    mutate(r_seconds = seconds(runif(1, min= 0, max = 50000))  ) %>% 
    mutate(Timestamp = as.POSIXct(Timestamp, format = "%m/%d/%Y %H:%M:%S") + months(6) + r_seconds) %>% 
    select(-r_seconds) %>% 
    mutate(Timestamp = as.character(paste0(month(Timestamp),"/", day(Timestamp), "/", year(Timestamp), " ", hour(Timestamp),":", minute(Timestamp),":", round(second(Timestamp)))) )%>% ungroup()  
  
wave_li <- append(list(a_wave), create_fake_wave(fake_wave, fake_waves-1) )
return(wave_li)
}

return(list(a_wave) )
}


#wave_li <- try(create_fake_wave(wave_df, 3))


#combine all datasets.
wave_df_li <- wave_li %>% 
      map(~analyze_dataset(.) )


#return list of symbols derived from the column names != "table_name"

data_by_wave <- wave_df_li %>% 
  bind_rows() 

bound_data <- data_by_wave %>% split(.$table_name) %>% map(~nest(unnest(.)) ) %>% 
  bind_rows(.id = "table_name") %>% 
  mutate(wave_id = "all")


##n_respondents_g
d <- bound_data %>% 
  filter(table_name == "n_respondents") %>%
  unnest() 

n_respondents<- d

n_respondents_gg <- ggplot(d, aes(y = n_completes, x = wave_date)) +
  geom_line() +
  geom_point(size = 6 , color  = "grey") +
  geom_text(aes(label = n_completes), nudge_x = 3, nudge_y = 3) +
  scale_x_date(date_labels = "%b %d, %Y", breaks = d$wave_date) +
  labs(x = "Survey Date", y = "Completed Surveys") +
  theme_tufte()

# n_respondents_gg
##


##enps
enps <- bound_data %>% 
  filter(table_name == "enps") %>%
  unnest() %>% 
  mutate(value_l = scales::percent(value %>% round(3))) 


coltbl <- bind_rows(
                    data.frame(measure = as.character(c(1:6)), color_group = "cg1") ,
                    data.frame(measure = as.character(c(7:8)), color_group = "cg2"),
                    data.frame(measure = as.character(c(9:10)), color_group = "cg3") ,
                    data.frame(measure = c("9 + 10","1 thru 6", "ENPS"), color_group = c("cg4", "cg5", "cg6") )
              ) %>% 
          mutate(measure = factor(measure, levels = levels(enps$measure)),
                 color_group = factor(color_group, levels = paste0("cg", c(1:6)) ) )

enps %<>% 
  left_join(coltbl) 

current_wave <- enps %>% filter(wave_date == max(wave_date))
current_wave_date <- current_wave$wave_date %>% max()
color_values = c("pink", "grey", "light green", "dark green", "dark red", "blue")


enps_current <- ggplot(current_wave, aes(y = value, x = measure)) +
  geom_col(aes(fill = color_group)) +
  geom_text(aes(label = value_l), nudge_y = .01) +
  scale_y_continuous(labels =scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = color_values) +
  labs(x = "Score", y = "Percent", title = paste0("Survey Wave: ", current_wave_date)) +
  theme_tufte() +
  theme(legend.position = "none") 

# enps_current

enps_over_time <-  ggplot(enps, aes(y = value, x = wave_date, group = measure, color  = color_group)) +
  geom_line() +
  geom_point(size = 6) +
  scale_x_date(date_labels = "%b %d, %Y", breaks = enps$wave_date) +
  scale_y_continuous(labels =scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = color_values) +
  labs(x = "Survey Date", y = "Percent") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_tufte() +
  theme(legend.position = "none") 

# enps_over_time
####

#distribution of all scores.
five_pt_dist_df <- bound_data %>% 
  filter(table_name == "overall_answer_percents") %>% 
  unnest() %>% 
  mutate(percent_l = scales::percent(p %>% round(3))) 

selected_overall_answers <- five_pt_dist_df %>% 
  filter(wave_date == max(wave_date))

# current_overall_gg <- ggplot(selected_overall_answers, aes(y = n, x = answer)) +
#   geom_col() +
#   geom_text(aes(label =n), nudge_y = -350 ) +
#   geom_text(aes(label =percent_l), nudge_y = 350) +
#   labs(x = "Response Category (5 pt scale)", y = "Count/Percent") +
#   ggtitle(paste0("Distribution of ", current_overall_answers$n %>% sum(), " Scores" ) )+
#   theme_tufte()

# current_overall_gg


#distribution of top2box scores over time
d <- bound_data %>% 
  filter(table_name == "top2box") %>% 
  unnest() %>% 
  mutate(percent_l = scales::percent(top2box %>% round(2), accuracy = 2 )) %>% 
  mutate(value = top2box)

d2 <-  bound_data %>% 
  filter(table_name == "bottom2box") %>% 
  unnest() %>% 
  mutate(percent_l = scales::percent(bottom2box %>% round(2), accuracy = 2))%>% 
  mutate(value = bottom2box)

d <- bind_rows(d, d2)

score_distribution_over_time_gg <- ggplot(d, aes(y = value, x = wave_date, group = table_name, color = table_name)) +
  geom_line() +
  geom_point(size = 6) +
  geom_text(aes(label = percent_l), nudge_y = .05) +
  scale_x_date(date_labels = "%b %d, %Y", breaks = d$wave_date) +
  labs(x = "Survey Date", y = "Percent") +
  scale_y_continuous(labels =scales::percent_format(accuracy = 1)) +
  theme_tufte() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("Percent Top and Bottom 2 Box Selected")

# score_distribution_over_time_gg
##

#performance categories table
perform_categories <- bound_data %>% 
  filter(table_name == "group_level_summary") %>% 
  unnest()

# create_table_for_categories <- function(d) {
#   d %>% 
#     select(Grouping, `Average Score` = aggregate_average_score) %>%
#     mutate(`Average Score` = round(`Average Score`, 2)) %>% 
#     arrange(desc(`Average Score`)) %>% 
#     return()
#   
# }

categories_table_current_kable <- perform_categories %>% 
  filter(as.character(wave_date) == max(wave_date)) %>% 
  select(Grouping, `Average Score` = aggregate_average_score) %>%
  mutate(`Average Score` = round(`Average Score`, 2)) %>% 
  arrange(desc(`Average Score`) )

categories_table_over_time_gg <-perform_categories %>% 
  select(wave_date, Grouping,`Average Score` = aggregate_average_score) %>% 
  ggplot(aes(x = wave_date, y = `Average Score`, color = Grouping)) +
  geom_line() +
  geom_point(size = 3) +
  coord_cartesian(ylim = c(1, 5)) +
  scale_x_date(date_labels = "%b %d, %Y", breaks = d$wave_date) +
  labs(x =  "Survey Date", y ="Average Score" ) +
  theme_tufte()

# categories_table_over_time_gg


#distribution of average scores (across respondents)
dist_across_respondents_5pt_hist <-  bound_data %>% 
  filter(table_name == "avg_respondent_5_pt") %>% 
  unnest() 

dist_across_respondents_5pt_hist_cut <- dist_across_respondents_5pt_hist %>% 
  mutate(answer_cut = cut(avg_answer, breaks = seq(1,5, by = .25), labels = seq(1.25, 5, by  = .25 )) ) %>% 
  count(wave_date, answer_cut) %>% 
  group_by(wave_date) %>% 
  mutate(p = n/sum(n)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  spread(answer_cut, p, fill = 0) %>% 
  gather(answer_cut, p, -wave_date)

# dist_across_respondents_5pt <- d %>% 
#   ggplot(aes(x = avg_answer, group = wave_date, fill = as.factor(wave_date)) ) +
#   geom_density(kernel = "gaussian", alpha = .5) +
#   coord_cartesian(xlim = c(1,5)) +
#   labs(x = "Average Answer", y = "Response Density", fill = "Wave") +
#   theme_tufte() 

# dist_across_respondents_5pt_hist <- dist_across_respondents_5pt_hist_cut %>% 
#   ggplot(aes(y = p, x = answer_cut, group = wave_date, fill = as.factor(wave_date)) ) +
#   geom_col(position = "dodge") +
#   coord_cartesian(ylim = c(0,1)) +
#   labs(x = "Average Answer", y = "Percent of Scores", fill = "Wave") +
#   scale_x_discrete(drop=FALSE) +
#   theme_tufte() 

# dist_across_respondents_5pt_hist_current <- d_cut %>% 
#   filter(wave_date == max(wave_date)) %>% 
#   ggplot(aes(y = p, x = answer_cut) ) +
#   geom_col(position = "dodge") +
#   coord_cartesian(ylim = c(0,1)) +
#   labs(x = "Average Answer", y = "Percent of Scores", fill = "Wave") +
#   scale_x_discrete(drop=FALSE) +
#   theme_tufte() 

# dist_across_respondents_5pt_hist_current

# how many five_point scales are there.

n_5_scale_qs <- bound_data %>% 
  filter(table_name == "five_pt_scale_data") %>% 
  unnest() %>% 
  filter(wave_date == max(wave_date)) %>% 
  select(q_num) %>% 
  distinct() %>% 
  nrow()



##
oe <- bound_data %>% 
  filter( table_name == "summary_oe_data") %>% 
  unnest() 

# current_oe <- oe %>% 
#   filter(wave_date == max(wave_date)) 
# 
# current_oe_gg <- current_oe %>% 
#   ggplot(aes(x = question_text, y = p, label = answer, color =question_text, text =sprintf("Answer: %s<br>Percent Selected: %s", answer, scales::percent(p)))) +
#   geom_jitter(aes(size = p), alpha = .5 ) +
#   labs(x = "", y = "Percent Answers in Category", title = paste0("Survey Wave: ", current_oe$wave_date[[1]] )) +
#   theme_minimal() +
#   coord_flip() +
#   theme(legend.position = "none") +
#   scale_y_continuous(labels = scales::percent) +
#   coord_cartesian(ylim = c(0, .7)) +
#   coord_flip()
# 
# current_oe_ggpltly <-   ggplotly(current_oe_gg, tooltip = "text")


overtime_oe_gg <- oe %>% 
  ggplot(aes(x = wave_date, y = p, label = answer, group = answer, color = answer,  text =sprintf("Answer: %s<br>Percent Selected: %s", answer, scales::percent(p)))) +
  geom_line(alpha = .2) +
  geom_point(position = position_dodge(width = 10), alpha = .2, size =3 ) +
  labs(x = "Survey Date", y = "Percent Answers in Category", title = "Over Time") +
  theme_minimal() + 
  scale_x_date(date_labels = "%b %d, %Y", breaks = unique(oe$wave_date) ) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") +
  facet_wrap(~question_text, ncol = 1) +
  coord_cartesian(ylim = c(0, .7))

overtime_oe_ggpltly <- ggplotly(overtime_oe_gg, tooltip = "text", height = 500)
overtime_oe_ggpltly

####

avg_question_score_current <- bound_data %>% 
  filter(table_name == "avg_question_score") %>% 
  unnest() %>% 
  filter(wave_date == max(wave_date)) %>% 
  arrange(avg_answer) 

by_question_distribution <- bound_data %>% 
  filter(table_name == "five_pt_scale_data") %>% 
  unnest() %>% 
  count(wave_date, q_num, question_text, answer) %>% 
  group_by(wave_date, q_num, question_text) %>% 
  mutate(p = n/sum(n)) %>% 
  ungroup()  %>% 
  select(-n) %>% 
  spread(answer, p, fill = 0) %>% 
  gather(key = "answer", value = "p", as.character(1:5)) %>% 
  mutate(question_text = factor(question_text, ordered = T, levels =avg_question_score_current$question_text ),
         answer = factor(answer))

# by_question_distribution_gg <- by_question_distribution %>% 
#   filter(wave_date == max(wave_date)) %>% 
#   ggplot(aes(x = answer, y = question_text, fill = p)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   labs(y = "")
# 
# by_question_distribution_ggpltly <- ggplotly(by_question_distribution_gg)

question_txt_li <- levels(by_question_distribution$question_text)

wave_dates <- by_question_distribution$wave_date %>% unique()

# names(question_txt_li) = question_txt_li 
