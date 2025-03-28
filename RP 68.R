#RP 1968 (Fichier détail REGION)

#O) Packages 

library(arrow)
library(haven)        # For reading .dta files
library(dplyr)        # For data manipulation (mutate, case_when, group_by, etc.)
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, etc.
library(janitor)      # For cleaning data, e.g., renaming variables
library(summarytools) # For frequency tables (freq)
library(reshape2)     # For reshaping data (melt, cast)
library(stargazer)    # For regression tables (if needed)
library(plm)          # For panel data models (if needed)

#I) Data --------------------------------------------------------------------------

#RP 1968

data_1968 <- read_sas("C:/Users/srimling/Documents/Positron/RP/RP 1968/Data/verdugo_rp68_fdq_14.sas7bdat", col_select = c("IN", "N", "DIP", "D", "PN", "REDI")) 

#II) Variables ------------------------------------------

# Nationality status

data_1968 <- data_1968 %>%
  mutate(Immigrant = ifelse(IN == 3, 1, 0))

freq(data_1968$Immigrant)

data_1968 <- data_1968 %>%
  mutate(French = ifelse(IN == 1, 1, 0))

freq(data_1968$French)

data_1968 <- data_1968 %>%
  mutate(Naturalized = ifelse(IN == 2, 1, 0))

freq(data_1968$Naturalized)



# Diploma

freq(data_1968$DIP)

data_1968 <- data_1968 %>%
  mutate(
    Low_Educ = ifelse(DIP %in% c(00, 10, 11, 20, 21, 22, 23), 1, 0),
    Mid_Educ = ifelse(DIP %in% c(30, 31, 42, 43, 44), 1, 0),
    High_Educ = ifelse(DIP %in% c(40, 41, 45, 50), 1, 0)
  )

freq(data_1968$Low_Educ)
freq(data_1968$Mid_Educ)
freq(data_1968$High_Educ)

# Nationalities 

freq(data_1968$PN)

unique(data_1968$PN)

data_1968 <- data_1968 %>%
  mutate(
    South_Europe = ifelse(PN %in% c("06", "11", "16"), 1, 0),  # Spain, Italy, Portugal
    Maghreb = ifelse(PN %in% c("30", "31", "45", "52"), 1, 0),     # Algeria, Morocco, Tunisia
    Europe = ifelse(PN %in% c("01", "02", "03", "05", "07", "08", "09", "10", "12", "13", "14", "18", "19", "20"), 1, 0),  # Western and North European countries
    East_Europe = ifelse(PN %in% c("04", "15", "17", "21", "22", "29"), 1, 0),  # Bulgaria, Poland, Romania, Czechoslovakia, Yugoslavia
    Asia = ifelse(PN %in% as.character(71:85), 1, 0),  # Asian countries (Vietnam, Japan, etc.)
    North_America = ifelse(PN %in% c("60", "61"), 1, 0),  # Canada, United States
    South_America = ifelse(PN %in% as.character(62:69), 1, 0),  # Argentina, Brazil, Chile, Peru, Venezuela
    Africa = ifelse(PN %in% as.character(c(32:44, 46:51, 59)), 1, 0),  # Others African countries
    Oceania = ifelse(PN %in% c("86", "87", "89"), 1, 0)  # Oceanian countries
  )

freq(data_1968$Asia)

# Department 

freq(data_1968$D)

data_1968$Department <- as.numeric(data_1968$D)

freq(data_1968$Department)

# Share 1968 

# First, calculate the numerator (department-level shares)

numerator_1968 <- data_1968 %>%
  group_by(Department, Europe, South_Europe, East_Europe, 
           Africa, Maghreb, Asia, North_America, South_America, Oceania,
           Low_Educ, Mid_Educ, High_Educ) %>%
  summarise(imm_nei = sum(Immigrant)) %>%
  ungroup()

# Calculate the denominator (national-level shares)

denominator_1968 <- data_1968 %>%
  group_by(Europe, South_Europe, East_Europe, 
           Africa, Maghreb, Asia, North_America, South_America, Oceania,
           Low_Educ, Mid_Educ, High_Educ) %>%
  summarise(imm_ne = sum(Immigrant)) %>%
  ungroup()

# Merge numerator and denominator

share_1968 <- numerator %>%
  left_join(denominator, by = c("Europe", "South_Europe", "East_Europe", 
                                "Africa", "Maghreb", "Asia", "North_America", 
                                "South_America", "Oceania",
                                "Low_Educ", "Mid_Educ", "High_Educ")) %>%
  mutate(share = imm_nei / imm_ne)

# Now aggregate across nationality and education groups for each department

share <- share_1968 %>%
  group_by(Department) %>%
  summarise(IV = sum(share, na.rm = TRUE))

# Merge back with original data if needed

data_1968 <- data_1968 %>%
  left_join(share, by = "Department")




# FB and NFB 1968 fractions 

library(dplyr)

# First, let's create the FB and NFB fractions for 1968
pop_component <- data_1968 %>%
  # Create indicators for FB and NFB (assuming "French" and "Naturalized" are binary)
  mutate(FB = as.numeric(French == 1),
         NFB = as.numeric(Naturalized == 1)) %>%
  
  # Calculate the 1968 shares for each education group at department and national levels
  group_by(Department, Low_Educ, Mid_Educ, High_Educ) %>%
  mutate(
    # Department-level counts in 1968 (numerators)
    FB_i1968 = sum(FB),
    NFB_i1968 = sum(NFB)
  ) %>%
  group_by(Low_Educ, Mid_Educ, High_Educ) %>%
  mutate(
    # National-level counts in 1968 (denominators)
    FB_1968 = sum(FB),
    NFB_1968 = sum(NFB)
  ) %>%
  ungroup() %>%
  
  # Compute the 1968 shares
  mutate(
    FB_share_1968 = FB_i1968 / FB_1968,
    NFB_share_1968 = NFB_i1968 / NFB_1968
  ) #%>%
  








  # For the current year (t), calculate the national counts by education
  group_by(Low_Educ, Mid_Educ, High_Educ) %>%
  mutate(
    FB_t = sum(FB),
    NFB_t = sum(NFB)
  ) %>%
  ungroup() %>%
  
  # Compute the final components
  mutate(
    FB_component = FB_share_1968 * FB_t,
    NFB_component = NFB_share_1968 * NFB_t
  ) %>%
  
  # Aggregate by department
  group_by(Department) %>%
  summarise(
    FB_shift_share = sum(FB_component, na.rm = TRUE),
    NFB_shift_share = sum(NFB_component, na.rm = TRUE),
    Total_population = FB_shift_share + NFB_shift_share
  )

# Merge back with original data if needed
data_1968 <- data_1968 %>%
  left_join(pop_component, by = "Department")




### Previous RP82 code

#Pondération (SOND)

freq(data$SOND)

#Region (R)

freq(data$R)

data <- data %>%
  mutate(Region = recode(R,
    "11" = "Ile-de-France",
    "21" = "Champagne-Ardenne",
    "22" = "Picardie",
    "23" = "Haute-Normandie",
    "24" = "Centre",
    "25" = "Basse-Normandie",
    "26" = "Bourgogne",
    "31" = "Nord-Pas-de-Calais",
    "41" = "Lorraine",
    "42" = "Alsace",
    "43" = "Franche-Comté",
    "52" = "Pays de la Loire",
    "53" = "Bretagne",
    "54" = "Poitou-Charentes",
    "72" = "Aquitaine",
    "73" = "Midi-Pyrénées",
    "74" = "Limousin",
    "82" = "Rhône-Alpes",
    "83" = "Auvergne",
    "91" = "Languedoc-Roussillon",
    "93" = "Provence-Alpes-Côte d'Azur",
    "94" = "Corse"
  ))

freq(data$Region)

#Genre (S)

freq(data$S)

data <- data %>%
  mutate(Gender = recode(S,
         "1" = "Homme",
         "2" = "Femme"))

freq(data$Gender)

data <- data %>%
  mutate(Male = case_when(
    S %in% c("1") ~ 1,
    TRUE ~ 0
  ))

#Immigrant Status (IN)

freq(data$IN)

data <- data %>%
  mutate(Nationality = recode(IN,
         "11" = "French",
         "12" = "French",
         "20" = "Immigrant"))

data <- data %>%
  mutate(Immigrant = case_when(
    IN %in% c("20") ~ 1,
    TRUE ~ 0
  ))

sum(data$Immigrant * data$SOND, na.rm = TRUE) #Nbr d'immigrés avec pondération au 1/4 (3 714 200)
sum(data$Immigrant * data$SOND, na.rm = TRUE) / sum(data$SOND, na.rm = TRUE) #Part d'immigrés dans la pop totale (~ 7%)

#Nationality

freq(data$N)

# Load dplyr package
library(dplyr)

data <- data %>%
  mutate(Nationality = recode(N,
    "01" = "Europe",
    "02" = "Europe",
    "03" = "Europe",
    "04" = "Europe",
    "05" = "Europe",
    "06" = "Espagnols",
    "07" = "Europe",
    "08" = "Europe",
    "09" = "Europe",
    "10" = "Europe",
    "11" = "Europe",
    "12" = "Europe",
    "13" = "Europe",
    "14" = "Europe",
    "15" = "Europe",
    "16" = "Portugais",
    "17" = "Europe",
    "18" = "Europe",
    "19" = "Europe",
    "20" = "Europe",
    "21" = "Europe",
    "22" = "Europe",
    "23" = "Europe",
    "24" = "Europe",
    "25" = "Europe",
    "29" = "Europe",
    "31" = "Maghreb",
    "33" = "Afrique",
    "34" = "Afrique",
    "35" = "Afrique",
    "36" = "Afrique",
    "37" = "Afrique",
    "39" = "Afrique",
    "40" = "Afrique",
    "42" = "Afrique",
    "43" = "Afrique",
    "44" = "Afrique",
    "45" = "Maghreb",
    "46" = "Afrique",
    "47" = "Afrique",
    "48" = "Afrique",
    "49" = "Afrique",
    "50" = "Afrique",
    "51" = "Afrique",
    "52" = "Maghreb",
    "53" = "Afrique",
    "54" = "Afrique",
    "55" = "Afrique",
    "56" = "Afrique",
    "57" = "Afrique",
    "59" = "Afrique",
    "60" = "AmNord",
    "61" = "AmNord",
    "62" = "AmNord",
    "63" = "AmSud",
    "64" = "AmSud",
    "65" = "AmSud",
    "66" = "AmSud",
    "67" = "AmSud",
    "68" = "AmSud",
    "69" = "AmSud",
    "70" = "Asie",
    "71" = "Asie",
    "72" = "Asie",
    "73" = "Asie",
    "74" = "Asie",
    "75" = "Asie",
    "76" = "Asie",
    "77" = "Asie",
    "78" = "Asie",
    "79" = "Asie",
    "80" = "Turquie",
    "81" = "Asie",
    "82" = "Asie",
    "83" = "Asie",
    "84" = "Asie",
    "85" = "URSS",
    "86" = "Océanie",
    "89" = "Océanie" ))

freq(data$Nationality)

data <- data %>%
  mutate(NatRegularized = case_when(
    Nationality %in% c("Portugais", "Espagnols", "Maghreb", "Afrique", "Turquie") ~ 1,
    TRUE ~ 0
  ))

freq(data$NatRegularized)

#Age (AD)

freq(data$AD)

data$Age <- data$AD

#Activity Status (TA)

freq(data$TA)

data <- data %>%
  mutate(Activity = recode(TA,
         "0" = "Inactif",
         "1" = "Actif occupé",
         "3" = "Chômage", 
         "4" = "Retraité",
         "5" = "Inactif",
         "6" = "Inactif"))

freq(data$Activity)

data <- data %>%
  mutate(Active = case_when(
    TA %in% c("1") ~ 1,
    TRUE ~ 0
  ))

#Matrimonial Status

data <- data %>%
  mutate(Matrimonial = recode(M,
         "1" = "Célibataire",
         "2" = "Marié",
         "3" = "Veuf",
         "4" = "Divorcé"))

freq(data$Matrimonial)  

data <- data %>%
  mutate(Celib = case_when(
    M %in% c("1") ~ 1,
    TRUE ~ 0
  ))

freq(data$Celib)

#Diploma

freq(data$DIPC)

data <- data %>%
  mutate(LowEduc = case_when(
    DIPC %in% c("0", "1", "2") ~ 1,
    TRUE ~ 0
  ))

freq(data$LowEduc)

#Economic Status (AE100)

freq(data$AE100)

data <- data %>%
  mutate(RegularizedSector = case_when(
    AE100 %in% c("01", "02", "03", "44", "45", "46", "47", "55", "57", "58", "59",
                 "60", "61", "62", "63", "64", "67", "98") ~ 1,
    TRUE ~ 0
  ))

freq(data$RegularizedSector)

#Dummy for ImmiRegularized

library(dplyr)

data <- data %>%
  mutate(
    ImmiRegularized = case_when(
      Age < 32 & 
      Immi == 1 & 
      Celib == 1 & 
      NatRegularized == 1 & 
      RegularizedSector == 1 & 
      Male == 1 & 
      Active == 1 & 
      LowEduc == 1 & 
      Region %in% c("Ile-de-France", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes") ~ 1,
      TRUE ~ 0 
    ))

freq(data$ImmiRegularized)



data <- data %>%
  mutate(
    ImmiRegularized = case_when(
      Age < 32 & 
      Immi == 1 & 
      Celib == 1 & 
      NatRegularized == 1 & 
      RegularizedSector == 1 & 
      Male == 1 & 
      Active == 1 & 
      LowEduc == 1 & 
      Region %in% c("Ile-de-France", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes") ~ 1,
      TRUE ~ 0 
    ))


data <- data %>%
  mutate(
    Illegals = case_when(
      Age < 32 & 
      Immi == 1 & 
      Celib == 1 & 
      NatRegularized == 1 & 
      RegularizedSector == 1 & 
      Male == 1 & 
      Active == 1 & 
      LowEduc == 1 & 
      Region %in% c("Ile-de-France", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes") ~ 1,
      TRUE ~ 0 
    ))

freq(data$IN)

freq(data$IMA)
freq(data$PRA)

freq(data$IRA75)
freq(data$IRA)

data <- data %>%
  mutate(
    Illegals = case_when(
     IN == "20" ~ 1,
     IMA == "3" ~ 1,
     IRA == "4" ~ 4,
     IRA75 == "4" ~ 4,
      TRUE ~ 0 
    ))


data <- data %>%
  mutate(
    Illegals = case_when(
      IN == "20" ~ 1,               
      IMA == "3" ~ 1,               
      IRA == "4" ~ 4,               
      IRA75 == "4" ~ 4,              
      N %in% c("31", "33", "34", "35", "36", "37", "39", "40", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "59") ~ 1, # Africa
      N %in% c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84") ~ 1, # Asian nationalities
      N %in% c("16", "06", "22") ~ 1, # Portuguese, Spanish, and Yougoslavians
      TRUE ~ 0 
    )
  )

sum(data$Illegals * data$SOND, na.rm = TRUE)

data <- data %>%
  mutate(
    Illegals = case_when(
      IN == "20" & IMA == "3" & IRA == "4" & IRA75 == "4" & TA == "1" &
      N %in% c("31", "33", "34", "35", "36", "37", "39", "40", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "59") | 
      N %in% c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84") | 
      N %in% c("16", "06", "22") ~ 1,
      TRUE ~ 0 
    )
  )

class(data$IN)
class(data$IMA)
class(data$IRA)
class(data$IRA75)
class(data$N)

#write_parquet(data, "dataRP1968.parquet")
