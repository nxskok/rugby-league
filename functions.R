read_scores <- function(worksheet, sheet) {
  scores0 <- read_excel(worksheet, sheet)
  scores0 %>% pivot_longer(-t1, names_to = "t2", values_to = "score") %>% 
    drop_na(score) %>% 
    filter(!str_detect(score, "x")) %>% 
    separate(score, into = c("s1", "s2"), sep = " ", convert = TRUE) %>% 
    select(t1, t2, s1, s2)
}


make_lookup_table <- function(scores) {
  scores %>% 
    pivot_longer(-c(s1, s2), names_to = "venue", values_to = "team") %>% 
    count(team) %>% 
    select(-n) %>% 
    mutate(id = row_number())
}