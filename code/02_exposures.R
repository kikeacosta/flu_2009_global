# rm(list=ls())
source("code/00_functions.R")

dts <- 
  read_rds("data_inter/stmf.rds")

unique(dts$Age)
unique(dts$Sex)
# looking for countries in the HMD
hmd_codes <- 
  read_csv("data_input/country_codes_hmd.csv") 


cts_hmd <- 
  dts %>% 
  left_join(hmd_codes %>% rename(country = Country)) %>% 
  pull(PopCode) %>% unique()

hmd_us <- Sys.getenv("hmd_us")
hmd_pw <- Sys.getenv("hmd_pw")

hmd_exps <- tibble()
for(ct in cts_hmd){
  chunk_p <- 
    readHMDweb(ct, "Exposures_1x1", hmd_us, hmd_pw) %>%
    filter(Year >= 2000) %>%
    as_tibble() %>%
    mutate(Code = ct)
  
  hmd_exps <- hmd_exps %>%
    bind_rows(chunk_p)
}

unique(hmd_exps$Age)

# selecting countries with data in 2020
pop <- 
  hmd_exps %>% 
  filter(!Code %in% c(
    # "GBRCENW", "GBRTENW", "GBR_SCO", "GBR_NIR",
                      "DEUTE", "DEUTW", 
                      "FRACNP", 
                      "NZL_NM", "NZL_MA")) %>% 
  ungroup() %>% 
  select(-OpenInterval) %>% 
  gather(Female, Male, Total, key = Sex, value = Population) %>% 
  mutate(Age = ifelse(Age < 90, Age - Age%%5, 90)) %>% 
  group_by(Code, Year, Sex, Age) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  mutate(Sex = recode(Sex,
                      "Female" = "f",
                      "Male" = "m",
                      "Total" = "t"),
         Code = case_when(Code == "GBR_NP" ~ "GBR",
                          Code == "NZL_NP" ~ "NZL",
                          Code == "FRATNP" ~ "FRA",
                          Code == "DEUTNP" ~ "DEU",
                          TRUE ~ Code),         
         Country = countrycode(Code, origin = "iso3c",
                               destination = "country.name"),
         Country = case_when(Code == "USA" ~ "USA",
                             TRUE ~ Country)) %>%   
  rename(country = Country,
         code = Code,
         year = Year,
         sex = Sex,
         age = Age,
         pop = Population) %>% 
  mutate(week = 26)

cds <- unique(pop$code) %>% sort
sxs <- unique(pop$sex)
ags <- unique(pop$age)

pop_interpol <- 
  expand_grid(year = 2000:2022, week = 1:52, sex = sxs, age = ags, code = cds) %>% 
  bind_rows(expand_grid(year = c(2004, 2009, 2015, 2020), 
                        week = 53, 
                        sex = sxs, age = ags, code = cds)) %>% 
  left_join(pop %>% select(-country)) %>% 
  group_by(code, sex, age) %>% 
  arrange(year, week) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  group_by(code, sex, age) %>% 
  do(interpop(db = .data)) %>% 
  ungroup()

out <- 
  pop_interpol %>% 
  select(-pop) %>% 
  rename(pop = pop2)

write_rds(out, "data_inter/weekly_population.rds")
