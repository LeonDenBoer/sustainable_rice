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

#########################################################cambodia

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


#################################### spanje

spain$Q1FM <- as.numeric(spain$Q1FM)
spain$Q2FM <- as.numeric(spain$Q2FM)
spain$Q3FM <- as.numeric(spain$Q3FM)

spain$Q4PP <- as.numeric(spain$Q4PP)
spain$Q5PP <- as.numeric(spain$Q5PP)
spain$Q6PP <- as.numeric(spain$Q6PP)
spain$Q7PP <- as.numeric(spain$Q7PP)

spain$Q8aPP <- as.numeric(spain$Q8aPP)
spain$Q8bPP <- as.numeric(spain$Q8bPP)

spain$Q9PP <- as.numeric(spain$Q9PP)

spain$Q10WU <- as.numeric(spain$Q10WU)
spain$Q10aWU <- as.numeric(spain$Q10aWU)
spain$Q10bWU <- as.numeric(spain$Q10bWU)
spain$Q10cWU <- as.numeric(spain$Q10cWU)

spain$Q11WU <- as.numeric(spain$Q11WU)
spain$Q12WU <- as.numeric(spain$Q12WU)
spain$Q13WU <- as.numeric(spain$Q13WU)
spain$Q14WU <- as.numeric(spain$Q14WU)

spain$Q15NM <- as.numeric(spain$Q15NM)
spain$Q16NM <- str_remove_all(spain$Q16NM, "[abc]")
spain$Q16NM <- as.numeric(spain$Q16NM)
spain$Q17NM <- str_remove_all(spain$Q17NM, "[abc]")
spain$Q17NM <- as.numeric(spain$Q17NM)

spain$Q18-1PM <- str_remove_all(spain$Q18-1PM, "[abc]")
spain$Q18-1PM <- as.numeric(spain$Q18-1PM)
spain$Q18-2PM <- as.numeric(spain$Q18-2PM)
spain$Q18-3PM <- as.numeric(spain$Q18-3PM)
spain$Q18-4PM <- as.numeric(spain$Q18-4PM)
spain$Q18-5PM <- as.numeric(spain$Q18-5PM)
spain$Q18-6PM <- as.numeric(spain$Q18-6PM)


spain$Q19HP <- as.numeric(spain$Q19HP)
spain$Q20HP <- as.numeric(spain$Q20HP)

spain$Q20aHP <- as.numeric(spain$Q20aHP)
spain$Q20bHP <- as.numeric(spain$Q20bHP)

spain$Q21HP <- as.numeric(spain$Q21HP)
spain$Q22HP <- as.numeric(spain$Q22HP)
spain$Q23HP <- as.numeric(spain$Q23HP)
spain$Q24HP <- as.numeric(spain$Q24HP)
spain$Q25HP <- as.numeric(spain$Q25HP)

spain$Q26HS <- as.numeric(spain$Q26HS)
spain$Q27HS <- as.numeric(spain$Q27HS)
spain$Q28HS <- str_remove_all(spain$Q28HS, "[abc]")
spain$Q28HS <- as.numeric(spain$Q28HS)
spain$Q29HS <- str_remove_all(spain$Q29HS, "[abc]")
spain$Q29HS <- as.numeric(spain$Q29HS)
spain$Q30HS <- str_remove_all(spain$Q30HS, "[abc]")
spain$Q30HS <- as.numeric(spain$Q30HS)
spain$Q31HS <- str_remove_all(spain$Q31HS, "[abc]")
spain$Q31HS <- as.numeric(spain$Q31HS)
spain$Q32HS <- str_remove_all(spain$Q31HS, "[abc]")
spain$Q32HS <- as.numeric(spain$Q32HS)
spain$Q33HS <- str_remove_all(spain$Q33HS, "[abc]")
spain$Q33HS <- as.numeric(spain$Q33HS)
spain$Q34HS <- str_remove_all(spain$Q34HS, "[abc]")
spain$Q34HS <- as.numeric(spain$Q34HS)

spain$Q35LR <- as.numeric(spain$Q35LR)
spain$Q36LR <- as.numeric(spain$Q36LR)
spain$Q37LR <- as.numeric(spain$Q37LR)
spain$Q38LR <- as.numeric(spain$Q38LR)
spain$Q39LR <- as.numeric(spain$Q39LR)
spain$Q40LR <- as.numeric(spain$Q40LR)
spain$Q41LR <- as.numeric(spain$Q41LR)

#############################################india

india$Q1FM <- as.numeric(india$Q1FM)
india$Q2FM <- as.numeric(india$Q2FM)
india$Q3FM <- as.numeric(india$Q3FM)

india$Q4PP <- as.numeric(india$Q4PP)
india$Q5PP <- as.numeric(india$Q5PP)
india$Q6PP <- as.numeric(india$Q6PP)
india$Q7PP <- as.numeric(india$Q7PP)

india$Q8aPP <- as.numeric(india$Q8aPP)
india$Q8bPP <- as.numeric(india$Q8bPP)

india$Q9PP <- as.numeric(india$Q9PP)

india$Q10WU <- as.numeric(india$Q10WU)
india$Q10aWU <- as.numeric(india$Q10aWU)
india$Q10bWU <- as.numeric(india$Q10bWU)
india$Q10cWU <- as.numeric(india$Q10cWU)

india$Q11WU <- as.numeric(india$Q11WU)
india$Q12WU <- as.numeric(india$Q12WU)
india$Q13WU <- as.numeric(india$Q13WU)
india$Q14WU <- as.numeric(india$Q14WU)

india$Q15NM <- as.numeric(india$Q15NM)
india$Q16NM <- str_remove_all(india$Q16NM, "[abc]")
india$Q16NM <- as.numeric(india$Q16NM)
india$Q17NM <- str_remove_all(india$Q17NM, "[abc]")
india$Q17NM <- as.numeric(india$Q17NM)

india$Q18-1PM <- str_remove_all(india$Q18-1PM, "[abc]")
india$Q18-1PM <- as.numeric(india$Q18-1PM)
india$Q18-2PM <- as.numeric(india$Q18-2PM)
india$Q18-3PM <- as.numeric(india$Q18-3PM)
india$Q18-4PM <- as.numeric(india$Q18-4PM)
india$Q18-5PM <- as.numeric(india$Q18-5PM)
india$Q18-6PM <- as.numeric(india$Q18-6PM)


india$Q19HP <- as.numeric(india$Q19HP)
india$Q20HP <- as.numeric(india$Q20HP)

india$Q20aHP <- as.numeric(india$Q20aHP)
india$Q20bHP <- as.numeric(india$Q20bHP)

india$Q21HP <- as.numeric(india$Q21HP)
india$Q22HP <- as.numeric(india$Q22HP)
india$Q23HP <- as.numeric(india$Q23HP)
india$Q24HP <- as.numeric(india$Q24HP)
india$Q25HP <- as.numeric(india$Q25HP)

india$Q26HS <- as.numeric(india$Q26HS)
india$Q27HS <- as.numeric(india$Q27HS)
india$Q28HS <- str_remove_all(india$Q28HS, "[abc]")
india$Q28HS <- as.numeric(india$Q28HS)
india$Q29HS <- str_remove_all(india$Q29HS, "[abc]")
india$Q29HS <- as.numeric(india$Q29HS)
india$Q30HS <- str_remove_all(india$Q30HS, "[abc]")
india$Q30HS <- as.numeric(india$Q30HS)
india$Q31HS <- str_remove_all(india$Q31HS, "[abc]")
india$Q31HS <- as.numeric(india$Q31HS)
india$Q32HS <- str_remove_all(india$Q31HS, "[abc]")
india$Q32HS <- as.numeric(india$Q32HS)
india$Q33HS <- str_remove_all(india$Q33HS, "[abc]")
india$Q33HS <- as.numeric(india$Q33HS)
india$Q34HS <- str_remove_all(india$Q34HS, "[abc]")
india$Q34HS <- as.numeric(india$Q34HS)

india$Q35LR <- as.numeric(india$Q35LR)
india$Q36LR <- as.numeric(india$Q36LR)
india$Q37LR <- as.numeric(india$Q37LR)
india$Q38LR <- as.numeric(india$Q38LR)
india$Q39LR <- as.numeric(india$Q39LR)
india$Q40LR <- as.numeric(india$Q40LR)
india$Q41LR <- as.numeric(india$Q41LR)

######################################## pakistan

pakistan$Q1FM <- as.numeric(pakistan$Q1FM)
pakistan$Q2FM <- as.numeric(pakistan$Q2FM)
pakistan$Q3FM <- as.numeric(pakistan$Q3FM)

pakistan$Q4PP <- as.numeric(pakistan$Q4PP)
pakistan$Q5PP <- as.numeric(pakistan$Q5PP)
pakistan$Q6PP <- as.numeric(pakistan$Q6PP)
pakistan$Q7PP <- as.numeric(pakistan$Q7PP)

pakistan$Q8aPP <- as.numeric(pakistan$Q8aPP)
pakistan$Q8bPP <- as.numeric(pakistan$Q8bPP)

pakistan$Q9PP <- as.numeric(pakistan$Q9PP)

pakistan$Q10WU <- as.numeric(pakistan$Q10WU)
pakistan$Q10aWU <- as.numeric(pakistan$Q10aWU)
pakistan$Q10bWU <- as.numeric(pakistan$Q10bWU)
pakistan$Q10cWU <- as.numeric(pakistan$Q10cWU)

pakistan$Q11WU <- as.numeric(pakistan$Q11WU)
pakistan$Q12WU <- as.numeric(pakistan$Q12WU)
pakistan$Q13WU <- as.numeric(pakistan$Q13WU)
pakistan$Q14WU <- as.numeric(pakistan$Q14WU)

pakistan$Q15NM <- as.numeric(pakistan$Q15NM)
pakistan$Q16NM <- str_remove_all(pakistan$Q16NM, "[abc]")
pakistan$Q16NM <- as.numeric(pakistan$Q16NM)
pakistan$Q17NM <- str_remove_all(pakistan$Q17NM, "[abc]")
pakistan$Q17NM <- as.numeric(pakistan$Q17NM)

pakistan$Q18-1PM <- str_remove_all(pakistan$Q18-1PM, "[abc]")
pakistan$Q18-1PM <- as.numeric(pakistan$Q18-1PM)
pakistan$Q18-2PM <- as.numeric(pakistan$Q18-2PM)
pakistan$Q18-3PM <- as.numeric(pakistan$Q18-3PM)
pakistan$Q18-4PM <- as.numeric(pakistan$Q18-4PM)
pakistan$Q18-5PM <- as.numeric(pakistan$Q18-5PM)
pakistan$Q18-6PM <- as.numeric(pakistan$Q18-6PM)


pakistan$Q19HP <- as.numeric(pakistan$Q19HP)
pakistan$Q20HP <- as.numeric(pakistan$Q20HP)

pakistan$Q20aHP <- as.numeric(pakistan$Q20aHP)
pakistan$Q20bHP <- as.numeric(pakistan$Q20bHP)

pakistan$Q21HP <- as.numeric(pakistan$Q21HP)
pakistan$Q22HP <- as.numeric(pakistan$Q22HP)
pakistan$Q23HP <- as.numeric(pakistan$Q23HP)
pakistan$Q24HP <- as.numeric(pakistan$Q24HP)
pakistan$Q25HP <- as.numeric(pakistan$Q25HP)

pakistan$Q26HS <- as.numeric(pakistan$Q26HS)
pakistan$Q27HS <- as.numeric(pakistan$Q27HS)
pakistan$Q28HS <- str_remove_all(pakistan$Q28HS, "[abc]")
pakistan$Q28HS <- as.numeric(pakistan$Q28HS)
pakistan$Q29HS <- str_remove_all(pakistan$Q29HS, "[abc]")
pakistan$Q29HS <- as.numeric(pakistan$Q29HS)
pakistan$Q30HS <- str_remove_all(pakistan$Q30HS, "[abc]")
pakistan$Q30HS <- as.numeric(pakistan$Q30HS)
pakistan$Q31HS <- str_remove_all(pakistan$Q31HS, "[abc]")
pakistan$Q31HS <- as.numeric(pakistan$Q31HS)
pakistan$Q32HS <- str_remove_all(pakistan$Q31HS, "[abc]")
pakistan$Q32HS <- as.numeric(pakistan$Q32HS)
pakistan$Q33HS <- str_remove_all(pakistan$Q33HS, "[abc]")
pakistan$Q33HS <- as.numeric(pakistan$Q33HS)
pakistan$Q34HS <- str_remove_all(pakistan$Q34HS, "[abc]")
pakistan$Q34HS <- as.numeric(pakistan$Q34HS)

pakistan$Q35LR <- as.numeric(pakistan$Q35LR)
pakistan$Q36LR <- as.numeric(pakistan$Q36LR)
pakistan$Q37LR <- as.numeric(pakistan$Q37LR)
pakistan$Q38LR <- as.numeric(pakistan$Q38LR)
pakistan$Q39LR <- as.numeric(pakistan$Q39LR)
pakistan$Q40LR <- as.numeric(pakistan$Q40LR)
pakistan$Q41LR <- as.numeric(pakistan$Q41LR)

#############

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