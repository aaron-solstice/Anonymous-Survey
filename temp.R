coded_oes <- data_long %>% 
  filter(grepl("coded", question_text)) 


max_responses <- coded_oes %>% 
  mutate(item_count = stringr::str_count(answer, ",") + 1) %>% 
  .$item_count %>% max()

new_vars <- paste0("a", 1:max_responses)

coded_oes_long <- coded_oes %>% 
  separate(answer, into = new_vars, sep = ",") %>% 
  gather(new_vars, key = "n", value = "answer") %>% 
  mutate(answer= trimws(answer))
  
