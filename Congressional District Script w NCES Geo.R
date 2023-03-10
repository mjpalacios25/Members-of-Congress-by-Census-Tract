library("dplyr")
library("purrr")
library("readxl")
library("rebus")
library("stringr")
library("tidyr")
library("readr")

rm(list = ls())

#Read in files, and bind rows on Census File
# CensusFile <- list.files() %>% str_subset(pattern = START %R% "grf")

#adjusted for 2023 because NCES did not have 118th congressional districts matched with LEAs

CensusCongressionalFile <- read_xlsx("Census School District to 118th Congressional District.xlsx")
CensusCongressionalFile$GEOID_CD118_20 <- sprintf("%04d", CensusCongressionalFile$GEOID_CD118_20)
CensusCongressionalFile <- CensusCongressionalFile %>% separate(col = "GEOID_CD118_20", into = c("STATE", "CD"), 2)
CensusCongressionalFile$LEAID <- sprintf("%07d", CensusCongressionalFile$LEAID)
StateCodes <- read_xlsx("State Codes.xlsx")

#need recode of state abbrv. to state numbers
HouseMembers <- read_excel("House XML.xlsx") %>%
  select("statedistrict", "last_name" = "member-info.lastname", "first_name" = "member-info.firstname",
        "official_name" =  "member-info.official-name", "party" = "member-info.party", "state" =  "member-info.state",
        "district" = "member-info.district", "town+name" =  "member-info.townname") %>%
  separate(col = "statedistrict", into = c("State", "CongressionalDistrict"), sep = 2) %>%
  left_join(StateCodes, by = c("State" = "STUSAB")) %>%
  unite("HouseMemberwParty", "first_name", "last_name", "party", sep = ", ")

SenateMembers <- read_excel("Senate XML.xlsx") %>%
  select(1:5) %>%
  left_join(StateCodes, by = c("state" = "STUSAB")) %>%
  unite("SenateMemberwParty", "first_name", "last_name", "party", sep = ", ") %>%
  group_by(state) %>%
  mutate(Senator = seq_along(1:2)) %>%
  pivot_wider(id_cols = state, names_from = Senator, values_from = SenateMemberwParty, names_prefix = "Senator ")

CGCSDistricts <- read_xlsx("CGCS Districts 10.2022.xlsx") %>%
  select("LEAID", "LEA_NAME")
CGCSDistricts$LEAID <- sprintf("%07d", CGCSDistricts$LEAID)

#Filter Census File by CGCS District LEAIDs
# vars <- c("LEAID", "NAME_LEA22", "State", "STATE", "CD", "COUNT", "district", "town+name", "HouseMemberwParty", "Senator 1", "Senator 2")
vars <- c("LEAID", "LEA_NAME",  "State", "STATE", "CD", "district", "town+name", "HouseMemberwParty", "Senator 1", "Senator 2")

CensusFiltered <- CensusCongressionalFile %>%
  inner_join(CGCSDistricts, by =  c("LEAID")) %>%
  left_join(HouseMembers, by = c("STATE", "CD" = "CongressionalDistrict")) %>%
  left_join(SenateMembers, by = c("State" = "state")) %>%
  select(!!vars)
  

#Save File
write_csv(CensusFiltered, "CGCS Members of Congress.csv")
