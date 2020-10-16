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
cambodia$x3 <- as.integer(cambodia$x3) 

cambodia$Q27HS <- as.numeric(cambodia$Q27HS)

cambodia$Q21HP <- str_remove_all(cambodia$Q21HP, "[abc]")
cambodia$Q21HP <- as.numeric(cambodia$Q21HP)

##############cambodia ##

cambodia$Q1FM <- as.numeric(cambodia$Q1FM)
cambodia$Q2FM <- as.numeric(cambodia$Q2FM)
cambodia$Q3FM <- as.numeric(cambodia$Q3FM)

cambodia$Q4PP <- as.numeric(cambodia$Q4PP)
cambodia$Q5PP <- as.numeric(cambodia$Q5PP)
cambodia$Q6PP <- as.numeric(cambodia$Q6PP)
cambodia$Q7PP <- as.numeric(cambodia$Q7PP)

      cambodia$Q8aPP <- as.numeric(cambodia$Q8aPP)
cambodia$Q8bPP <- as.numeric(cambodia$Q8bPP)

cambodia$Q9PP <- as.numeric(cambodia$Q9PP)

      cambodia$Q10WU <- as.numeric(cambodia$Q10WU)
cambodia$Q10aWU <- as.numeric(cambodia$Q10aWU)
cambodia$Q10bWU <- as.numeric(cambodia$Q10bWU)
cambodia$Q10cWU <- as.numeric(cambodia$Q10cWU)

cambodia$Q11WU <- as.numeric(cambodia$Q11WU)
cambodia$Q12WU <- as.numeric(cambodia$Q12WU)
cambodia$Q13WU <- as.numeric(cambodia$Q13WU)
cambodia$Q14WU <- as.numeric(cambodia$Q14WU)

cambodia$Q15NM <- as.numeric(cambodia$Q15NM)
cambodia$Q16NM <- str_remove_all(cambodia$Q16NM, "[abc]")
cambodia$Q16NM <- as.numeric(cambodia$Q16NM)
cambodia$Q17NM <- str_remove_all(cambodia$Q17NM, "[abc]")
cambodia$Q17NM <- as.numeric(cambodia$Q17NM)

cambodia$Q18-1PM <- str_remove_all(cambodia$Q18-1PM, "[abc]")
cambodia$Q18-1PM <- as.numeric(cambodia$Q18-1PM)
cambodia$Q18-2PM <- as.numeric(cambodia$Q18-2PM)
cambodia$Q18-3PM <- as.numeric(cambodia$Q18-3PM)
cambodia$Q18-4PM <- as.numeric(cambodia$Q18-4PM)
cambodia$Q18-5PM <- as.numeric(cambodia$Q18-5PM)
cambodia$Q18-6PM <- as.numeric(cambodia$Q18-6PM)


cambodia$Q19HP <- as.numeric(cambodia$Q19HP)
      cambodia$Q20HP <- as.numeric(cambodia$Q20HP)

cambodia$Q20aHP <- as.numeric(cambodia$Q20aHP)
cambodia$Q20bHP <- as.numeric(cambodia$Q20bHP)

cambodia$Q21HP <- as.numeric(cambodia$Q21HP)
cambodia$Q22HP <- as.numeric(cambodia$Q22HP)
cambodia$Q23HP <- as.numeric(cambodia$Q23HP)
cambodia$Q24HP <- as.numeric(cambodia$Q24HP)
cambodia$Q25HP <- as.numeric(cambodia$Q25HP)

cambodia$Q26HS <- as.numeric(cambodia$Q26HS)
cambodia$Q27HS <- as.numeric(cambodia$Q27HS)
cambodia$Q28HS <- str_remove_all(cambodia$Q28HS, "[abc]")
cambodia$Q28HS <- as.numeric(cambodia$Q28HS)
cambodia$Q29HS <- str_remove_all(cambodia$Q29HS, "[abc]")
cambodia$Q29HS <- as.numeric(cambodia$Q29HS)
cambodia$Q30HS <- str_remove_all(cambodia$Q30HS, "[abc]")
cambodia$Q30HS <- as.numeric(cambodia$Q30HS)
cambodia$Q31HS <- str_remove_all(cambodia$Q31HS, "[abc]")
cambodia$Q31HS <- as.numeric(cambodia$Q31HS)
cambodia$Q32HS <- str_remove_all(cambodia$Q31HS, "[abc]")
cambodia$Q32HS <- as.numeric(cambodia$Q32HS)
cambodia$Q33HS <- str_remove_all(cambodia$Q33HS, "[abc]")
cambodia$Q33HS <- as.numeric(cambodia$Q33HS)
cambodia$Q34HS <- str_remove_all(cambodia$Q34HS, "[abc]")
cambodia$Q34HS <- as.numeric(cambodia$Q34HS)

cambodia$Q35LR <- as.numeric(cambodia$Q35LR)
cambodia$Q36LR <- as.numeric(cambodia$Q36LR)
cambodia$Q37LR <- as.numeric(cambodia$Q37LR)
cambodia$Q38LR <- as.numeric(cambodia$Q38LR)
cambodia$Q39LR <- as.numeric(cambodia$Q39LR)
cambodia$Q40LR <- as.numeric(cambodia$Q40LR)
cambodia$Q41LR <- as.numeric(cambodia$Q41LR)







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