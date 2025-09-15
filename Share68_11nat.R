#RP 1968 (INSEE)

#O) Packages 

library(arrow)
library(haven)        
library(dplyr)       
library(tidyverse)    
library(janitor)      
library(summarytools) 
library(reshape2)     
library(stargazer)    
library(plm)          

#I) Data 

data_1968 <- read_sas("C:/Users/srimling/Documents/Positron/RP/RP 1968/Data/verdugo_rp68_fdq_14.sas7bdat", col_select = c("IN", "N", "DIP", "D", "PN", "TA", "REDI"))

#II) Variables 

#A) Immigrant and Natives 

data_1968 <- data_1968 %>%
  mutate(Nationality = case_when(
    IN == 1 ~ "Native",
    IN == 2 ~ "Naturalized",
    IN == 3 ~ "Immigrant",
  )) %>%
  mutate(Nationality = factor(Nationality, levels = c("Native", "Naturalized", "Immigrant")))

#B) Nationality (11 nationalities)

freq(data_1968$N)

data_1968 <- data_1968 %>%
  mutate(Origin = case_when(
    N %in% c("31") ~ "Algeria",    
    N %in% c("45") ~ "Morocco",
    N %in% c("52") ~ "Tunisia",
    N %in% c("06") ~ "Spain",
    N %in% c("11") ~ "Italy",
    N %in% c("16") ~ "Portugal",
    N %in% c("80") ~ "Turkey",
    N %in% c("01", "02", "03", "05", "07", "08", "09", "10", 
              "12", "13", "14", "18", "19", "20",
              "04", "15", "17", "21", "22", "29", "85") ~ "Europe",  # Western/Northern + Eastern Europe + USSR 
    N %in% c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "81", "84", "86", "87", "89") ~ "Asia",  # Asia + Oceania
    N %in% c("60", "61", 
              "62", "63", "64", "65", "66", "67", "68", "69") ~ "America",  # North + South America
    N %in% c("32", "33", "34", "35", "36", "37", "38", "39", 
              "40", "41", "42", "43", "44", "46", "47", "48", 
              "49", "50", "51", "59") ~ "Africa",  # Other Africa
    TRUE ~ NA_character_ # Native French + Other (99)
  ))

freq(data_1968$Origin)

#C) Departement

data_1968$Departement <- as.factor(data_1968$D)

freq(data_1968$Departement)

#III) 1968 Share (for SS-IV) 

#A) Immigrant 1968 Share (numerator for SSIV)

immi_data <- data_1968 %>%
  filter(Nationality == "Immigrant") %>%  
  filter(!is.na(Origin), !is.na(Departement))

immi_counts <- immi_data %>%
  group_by(Departement, Origin) %>%
  summarise(
    n_indiv = 4 * n(),  # Weighted
    .groups = "drop"
  )

immi_totals <- immi_counts %>%
  group_by(Origin) %>%
  summarise(
    total_group = sum(n_indiv),  
    .groups = "drop"
  )

sum(immi_totals$total_group)

immi_shares <- immi_counts %>%
  left_join(immi_totals, by = c("Origin")) %>%
  mutate(
    share = n_indiv / total_group
  )

all_immi_combos <- expand_grid(
  Departement = unique(data_1968$Departement),
  Origin = unique(immi_data$Origin),
)

immi_share_rect <- all_immi_combos %>%
  left_join(immi_shares, by = c("Departement", "Origin")) %>%
  mutate(
    share = replace_na(share, 0),
    n_indiv = replace_na(n_indiv, 0),
    total_group = replace_na(total_group, 0)
  )

#B) Departemental population in 1968 (for the numerator)

pop_1968 <- data_1968 %>%
  group_by(Departement) %>%
  summarise(pop = n() * 4) %>%
  ungroup()


#IV) Final dataset

write_parquet(immi_share_rect, "shareimmi68_11nat.parquet")
write_parquet(pop_1968, "pop1968")
