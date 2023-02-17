rm(list=ls())
source("code/00_functions.R")

# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 25 May 2021
download.file("https://www.mortality.org/File/GetDocument/Public/STMF/Inputs/STMFinput.zip", 
              "data_input/STMFinput.zip")

# list of country codes in STMF
zipdf <- unzip("data_input/STMFinput.zip", list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- 
    read_csv(unz("data_input/STMFinput.zip", csv_file)) %>% 
    mutate(Week = Week %>% as.double,
           Deaths = Deaths %>% as.double())
  db_d <- 
    db_d %>% 
    bind_rows(temp)
}

unique(db_d$PopCode)


# countries with full 2020
db_d %>% 
  group_by(PopCode) %>% 
  filter(min(Year) <= 2004) %>% 
  pull(PopCode) %>% unique()

cts_2009 <- 
  db_d %>% 
  group_by(PopCode) %>% 
  filter(min(Year) <= 2004)

# countries with data in 2009
cts_2009 <- 
  db_d %>% 
  filter(Year == 2004) %>% 
  pull(PopCode) %>% unique()

dts <- 
  db_d %>% 
  mutate(PopCode = ifelse(PopCode == "a", "NOR", PopCode)) %>% 
  filter(Year %in% 2000:2021) %>% 
  filter(PopCode %in% cts_2009) %>% 
  select(-Access, -Type, -Area)


# countries with changes in age groups
unique_ages_year <- 
  dts %>% 
  select(PopCode, Age, AgeInterval) %>% 
  unique() %>% 
  group_by(PopCode, Age) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2)

dts2 <- 
  dts %>% 
  drop_na(Deaths) %>% 
  mutate(Age = case_when(PopCode %in% c("FRATNP") & Age == "1" ~ "0", 
                         PopCode %in% c("FRATNP") & Age == "95" ~ "90", 
                         PopCode %in% c("GBR_SCO") & Age == "95" ~ "90", 
                         TRUE ~ Age)) %>% 
  group_by(PopCode, Year, Week, Sex, Age) %>%
  summarise(Deaths = sum(Deaths)) %>%
  ungroup() %>%
  filter(Age != "UNK") %>% 
  mutate(Sex = recode(Sex,
                      "b" = "t"),
         PopCode = recode(PopCode,
                          "DEUTNP" = "DEU",
                          "FRATNP" = "FRA",
                          "NZL_NP" = "NZL")) %>%
  rename(Code = PopCode) 

# # re-scaling ages and sexes
# dts3 <-
#   dts2 %>%
#   group_by(Code, Sex, Year, Week) %>%
#   do(rescale_age(chunk = .data)) %>%
#   ungroup() %>%
#   group_by(Code, Age, Year, Week) %>%
#   do(rescale_sex(chunk = .data)) %>%
#   ungroup() %>%
#   mutate(Age = Age %>% as.double()) %>%
#   arrange(Code, Year, Week, Sex, Age)
# 
# dts4 <- 
#   dts3 %>% 
#   mutate(Country = countrycode(sourcevar = Code, 
#                                origin = "iso3c", 
#                                destination = "country.name")) %>% 
#   select(Country, Code, Year, Week, Sex, Age, Deaths) %>% 
#   arrange(Code, Year, Week, Sex, Age)
# 

dts4 <- 
  dts2 %>% 
  filter(Age != "TOT") %>% 
  mutate(Age = Age %>% as.integer(), 
         Country = countrycode(sourcevar = Code,
                               origin = "iso3c",
                               destination = "country.name")) %>%
  select(country = Country,
         code = Code,
         year = Year,
         week = Week,
         sex = Sex,
         age = Age,
         dts = Deaths) %>% 
  drop_na(week, country) %>% 
  mutate(isoweek = paste0(year, "-W", sprintf("%02d", week), "-7"),
         date = ISOweek2date(isoweek))


# saving data
write_rds(dts4, "data_inter/stmf.rds"); write_csv(dts4, "data_inter/stmf.csv")

