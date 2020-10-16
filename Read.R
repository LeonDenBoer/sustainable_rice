install.packages("readxl")
install.packages("stringr") 
install.packages("data.table")
install.packages("dplyr")# Install data.tablepackage
library("data.table")                              # Load data.table
library("dplyr")
library("readxl")
library(tidyverse)
library("stringr")  
library("data.table")    
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


spain$Q8cPP <- NULL
india$Q8cPP <- NULL
cambodia$Q8cPP <- NULL
pakistan$Q8cPP <- NULL

spain$Q10dWU <- paste(spain$Q10aWU, spain$Q10bWU, spain$Q10cWU)
india$Q10dWU <- paste(india$Q10aWU, india$Q10bWU, india$Q10cWU)
cambodia$Q10dWU <- paste(cambodia$Q10aWU, cambodia$Q10bWU, cambodia$Q10cWU)
pakistan$Q10dWU <- paste(pakistan$Q10aWU, pakistan$Q10bWU, pakistan$Q10cWU)

spain$Q10dWU <- str_remove_all(spain$Q10dWU, "[NA ]")
india$Q10dWU <- str_remove_all(india$Q10dWU, "[NA ]")
cambodia$Q10dWU <- str_remove_all(cambodia$Q10dWU, "[NA ]")
pakistan$Q10dWU <- str_remove_all(pakistan$Q10dWU, "[NA ]")
cambodia$Q9PP <- str_remove_all(cambodia$Q9PP, "[abc]")
cambodia[is.na(cambodia)] <- 0
cambodia <- replace_na(list(x = 0, y = "unknown"))

echte_punten <- data.frame("column" = c("Q1FM","Q2FM","Q3FM","Q4PP","Q5PP","Q6PP","Q7PP","Q8bPP","Q9PP","Q10dWU","Q11WU","Q12WU","Q13WU","Q14WU","Q15NM","Q16NM","Q17NM","Q18-1PM","Q18-2PM","Q18-3PM","Q18-4PM","Q18-5PM","Q18-6PM",
                                        "Q19HP","Q20HP","Q21HP","Q22HP","Q23HP","Q24HP","Q25HP","Q26HS","Q27HS","Q28HS","Q29HS","Q30HS","Q31HS","Q32HS","Q33HS","Q34HS","Q35LR","Q35LR","Q36LR","Q37LR","Q38LR","Q39LR","Q40LR",
                                        "Q41LR"), "max_points" = c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3))

max_pp_cat <- data.frame("group" = c("Farm Management", "Preplanting", "Water Use", "Nutrient Management", "Integrated Pest Management", "Harvest and Postharvest", "Health and Safety","Labor Rights"),
                          "max_group_points" = c(sum(echte_punten[echte_punten$column %like% "FM",]$max_points), sum(echte_punten[echte_punten$column %like% "PP",]$max_points), sum(echte_punten[echte_punten$column %like% "WU",]$max_points),
                                                 sum(echte_punten[echte_punten$column %like% "NM",]$max_points),sum(echte_punten[echte_punten$column %like% "PM",]$max_points),sum(echte_punten[echte_punten$column %like% "HP",]$max_points),sum(echte_punten[echte_punten$column %like% "HS",]$max_points),
                                                 sum(echte_punten[echte_punten$column %like% "LR",]$max_points)))


cambodia_punten <- data.frame("FM_punten"= c(sum(as.numeric(cambodia$Q1FM), cambodia$Q2FM, cambodia$Q3FM)))

sum(echte_punten$max_points)
sum(echte_punten[echte_punten$column %like% "FM",]$max_points)
c(sum(select(cambodia, contains("FM")))) 
       
cambodia[, grepl("FM", names(cambodia))]

for(i in 1:nrow(cambodia)){
  row <- cambodia[i,]
  new_row <- data.frame("FM_punten"= c(sum(row$Q1FM, row$Q2FM, row$Q3FM), "FM_procenten" = c(100 * sum(as.numeric(row$Q1FM), row$Q2FM, row$Q3FM) / sum(echte_punten[echte_punten$column %like% "FM",]$max_points)),
                        "PP_punten" = c(sum(row$Q4PP, row$Q5PP, row$Q6PP, row$Q7PP, row$Q8PP, row$Q9PP)), "PP_procenten" = c(100 * sum(row$Q4PP, row$Q5PP, row$Q6PP, row$Q7PP, row$Q8PP, row$Q9PP) / sum(echte_punten[echte_punten$column %like% "PP",]$max_points))))
}

for (i in 1:nrow(cleanTweets_disney)){
  row <- cleanTweets_disney[i,]
  sentiment <- select(analyzeSentiment(row$text), "SentimentGI", "NegativityGI", "PositivityGI")
  
  new_row <- cbind(row, sentiment)
  disney_sentiment2 <- rbind(disney_sentiment2, new_row, deparse.level = 1)
  
  if(i %% 100 == 0){
    cat(i)
    cat("..")
  }
}