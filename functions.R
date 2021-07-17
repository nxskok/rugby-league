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

get_draws <- function(fit) {
  draws_array <- fit$draws()
  draws <- as_draws_df(draws_array)
  draws %>% 
    pivot_longer(contains("rat[")) %>% 
    mutate(team = parse_number(name)) -> draws_with_team
  list(draws = draws, draws_with_team = draws_with_team)
}

read_fixtures <- function(workbook, sheet, lookup) {
  scores0 <- read_excel(workbook, sheet)
  scores0 %>% pivot_longer(-t1, names_to = "t2", values_to = "score") %>% 
    filter(str_detect(score, "x")) %>% 
    select(-score) %>% 
    left_join(lookup, by = c("t1" = "team")) %>% 
    left_join(lookup, by = c("t2" = "team"))
}

round_to_even <- function(x) {
  2 * round(x/2)
}

round_score <- function(d) {
  # d is df with m1 and m2 in it
  d %>% 
    mutate(s1 = round_to_even(m1),
           s2 = round_to_even(m2),
           g  = (m1 >= m2),
           s1 = ifelse(((s1 == s2) & g),  s1+1, s1),
           s2 = ifelse(((s1 == s2) & !g), s2+1, s2)) %>% 
    select(-g)
}

make_ppd <- function(draws, draws_with_team, t1_id, t2_id) {
  draws_with_team %>% 
    filter(team == t1_id) %>% 
    pull(value) -> rat1
  draws_with_team %>% 
    filter(team == t2_id) %>% 
    pull(value) -> rat2
  tibble(h = draws$h,
         k = draws$k,
         r1 = rat1,
         r2 = rat2) %>% 
    mutate(diff = r1 - r2) -> my_draws
  nr <- nrow(my_draws)
  my_draws %>% 
    mutate(eta1 = h + k + diff, 
           eta2 = k - diff,
           mu1 = exp(eta1),
           mu2 = exp(eta2),
           sc1 = rpois(nr, mu1),
           sc2 = rpois(nr, mu2)) %>% 
    select(sc1, sc2)
}

ppd_stats <- function(ppd) {
  # two columns, sc1 and sc2
  ppd %>% 
    mutate(res = case_when(
      sc1 > sc2  ~ "1",
      sc1 == sc2 ~ "X",
      TRUE       ~ "2"
    )) -> d
    d %>% count(res) %>% 
      deframe() -> v
    v <- v/sum(v)*100
    d %>% summarize(m1 = mean(sc1), m2 = mean(sc2)) -> dd
    list(prob = v, means = dd)
}