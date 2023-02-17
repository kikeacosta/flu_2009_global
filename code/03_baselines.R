# rm(list=ls())
source("code/00_functions.R")

dt <- 
  read_rds("data_inter/stmf.rds")

pp <- 
  read_rds("data_inter/weekly_population.rds")


dt2 <- 
  dt %>% 
  left_join(pp %>% select(-t)) %>% 
  filter(year < 2020)
  
tes <- 
  dt2 %>% 
  filter(is.na(pop))

cts_exc <- unique(tes$country)

dt3 <- 
  dt2 %>% 
  filter(!country %in% cts_exc,
         sex == "t") %>% 
  select(-sex) %>% 
  group_by(country, age) %>% 
  arrange(date) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(exposure = pop / 52)

dt3 %>% 
  filter(country == "France",
         age == 50) %>% 
  ggplot()+
  geom_line(aes(date, dts))

dt3 %>% 
  filter(country == "France",
         age == 30) %>% 
  ggplot()+
  geom_line(aes(date, dts))

bsn <- 
  dt3 %>% 
  group_by(country, age) %>% 
  do(est_linear(db = .data)) %>% 
  ungroup()

unique(bsn$country)
cts <- c("Spain", "France", "Belgium")

bsn %>% 
  mutate(mx = 1e4*dts/exposure) %>% 
  filter(country %in% cts,
         age %in% 50:55) %>% 
  ggplot()+
  geom_point(aes(date, mx))+
  # geom_line(aes(date, bsn))+
  # geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  theme_bw()+
  facet_wrap(age~country, scales = "free_y")+
  geom_vline(xintercept = ymd(c("2009-10-15", "2014-01-15")), linetype = "dashed")

bsn_mth <- 
  bsn %>% 
  mutate(mth = month(date)) %>%
  group_by(country, year, mth, age) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn),
            exposure = sum(exposure)) %>% 
  ungroup() %>% 
  mutate(mx = 1e5*dts/exposure,
         mx_bsn = 1e5*bsn/exposure,
         date = make_date(d = 15, month = mth, year = year))
  

bsn_mth %>% 
  filter(country %in% cts,
         age %in% 80:85) %>% 
  ggplot()+
  geom_point(aes(date, mx))+
  geom_line(aes(date, mx_bsn), col = "red")+
  # geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.2)+
  theme_bw()+
  facet_grid(age~country)+
  geom_vline(xintercept = ymd(c("2009-10-15", "2014-01-15")), linetype = "dashed")

