install.packages("readxl")
# Loading
library("readxl")
library(tidyverse)

# xlsx files
spain <- read_excel("ES-2019-SRP-ASSESMENT.xlsx")
india <- read_excel("ID-2019-SRP-ASSESMENT.xlsx")
pakistan <- read_excel("PK_2019_SRP_ASSESMENT.xlsx")
cambodia <- read_excel("KH_2019_SRP_ASSESMENT.xlsx")

india %>% rename(
  district = Tehsil,
  stad = Block,
  gemeente = Panchayat
)



