library(rvest)
library(magrittr)
library(data.table)
library(stringr)
library(tidyr)
library(dplyr)
library(XML)

bowls <- read_html("http://www.sports-reference.com/cfb/bowls/")
bowls <- html_table(html_nodes(bowls, "table#bowls")[[1]])[1:41, 1:3]

bowls$name <- tolower(bowls$Bowl) %>% 
  gsub(" ", "-", .) %>%
  gsub("'", "", .) %>%
  gsub("godaddy.com", "godaddycom", .) %>%
  gsub("autonation-", "", .) %>%
  gsub("popeyes-", "", .)
bowls$url <- bowls$name %>%
  paste("http://www.sports-reference.com/cfb/bowls/", ., ".html", sep="")

games <- matrix(ncol = 19, nrow = 0)
favorites <- matrix(ncol = 37, nrow = 0)
underdogs <- matrix(ncol = 37, nrow = 0)

for (i in 1:nrow(bowls)){
  
  # get games history table for bowl i
  temp_bowl <- read_html(bowls$url[[i]])
  temp_bowl <- html_table( 
    html_nodes(temp_bowl, paste("table#", bowls$name[[i]], sep = "") )[[1]] )[, 1:8]
  temp_bowl <- temp_bowl[temp_bowl$Year>=2000, ]#1968, ]
  temp_bowl$Bowl <- bowls$Bowl[[i]]  # add name of bowl game as a variable
  temp_bowl$`Winner/Tie` <- temp_bowl$`Winner/Tie` %>%
    sub("\\(([^)0-9]*)\\)", "\\1", .)
  temp_bowl$`Loser/Tie` <- temp_bowl$`Loser/Tie` %>%
    sub("\\(([^)0-9]*)\\)", "\\1", .)
  
  # separate team names and ranks
  temp_bowl$Winner <- str_split_fixed(temp_bowl$`Winner/Tie`, " \\(", 2)[, 1]
  temp_bowl$WinnerRank <- str_split_fixed(temp_bowl$`Winner/Tie`, " \\(", 2)[, 2] %>%
    gsub(")", "", .)
  temp_bowl$Loser <- str_split_fixed(temp_bowl$`Loser/Tie`, " \\(", 2)[, 1]
  temp_bowl$LoserRank <- str_split_fixed(temp_bowl$`Loser/Tie`, " \\(", 2)[, 2] %>%
    gsub(")", "", .)
  
  # ground truthing: favorite/underdog based on rank
  temp_bowl <- temp_bowl[!(temp_bowl$WinnerRank == "" & temp_bowl$LoserRank == ""), ]
  
  if( !nrow(temp_bowl) == 0 ){
    temp_bowl$WinnerRank <- as.numeric(temp_bowl$WinnerRank)
    temp_bowl$LoserRank <- as.numeric(temp_bowl$LoserRank)
    # create new variables for Favorite and Underdog
    temp_bowl$Favorite <- ifelse( is.na(temp_bowl$WinnerRank), temp_bowl$Loser, 
                                 ifelse( is.na(temp_bowl$LoserRank), temp_bowl$Winner, 
                                        ifelse(temp_bowl$WinnerRank < temp_bowl$LoserRank, temp_bowl$Winner,
                                               temp_bowl$Loser)))
    temp_bowl$Underdog <- ifelse(temp_bowl$Winner == temp_bowl$Favorite, temp_bowl$Loser, temp_bowl$Winner)
    
    # generate urls for the winning and losing teams
    temp_bowl$favoritename <- tolower(temp_bowl$Favorite) %>% 
      gsub(" ", "-", .) %>%
      gsub("&-", "", .) %>%
      gsub("&", "", .)
    temp_bowl$favoriteurl <- temp_bowl$favoritename %>% 
      paste("http://www.sports-reference.com/cfb/schools/", ., "/", temp_bowl$Year, ".html", sep="")
    temp_bowl$underdogname <- tolower(temp_bowl$Underdog) %>% 
      gsub(" ", "-", .) %>%
      gsub("&-", "", .) %>%
      gsub("&", "", .)
    temp_bowl$underdogurl <- temp_bowl$underdogname %>% 
      paste("http://www.sports-reference.com/cfb/schools/", ., "/", temp_bowl$Year, ".html", sep="")
    
    # for each game in the history of a given bowl
    for (j in 1:nrow(temp_bowl)){
      favorite_html <- read_html(temp_bowl$favoriteurl[[j]])
      
      # get season stats
      favorite <- html_table( 
        html_nodes(favorite_html, "table#team")[[1]] )[1:2, ]
      
      #remove some columns, rename columns, and combine to one row
      favorite <- subset(favorite, select = -c(1, 2, 13, 18, 23))
      colnames(favorite) <- c("PassComp", "PassAtt", "PassPct", "PassYds", "PassTD",
                              "RushAtt", "RushYds", "RushYdsPerAtt", "RushTD",
                              "Plays", "YdsPerPlay", 
                              "Pass1st", "Rush1st", "Penalty1st",
                              "Penalties", "PenaltyYds", 
                              "Fum", "Int")
      favorite <- cbind(favorite[1, ], favorite[2, ])
      
      for (k in 1:ncol(favorite)){
        if (k <= ncol(favorite)/2){
          colnames(favorite)[[k]] <- paste("F.O.", colnames(favorite)[[k]], sep="")
        }else{
          colnames(favorite)[[k]] <- paste("F.D.", colnames(favorite)[[k]], sep="")
        }
      }
      
      # get season record
      favorite_header <- html_nodes(favorite_html, "#meta")
      favorite_header <- html_text(favorite_header)
      record <- strsplit(favorite_header, "Record: ")[[1]][2]
      record <- str_match(record, "(\\d+)-(\\d+) \\((\\d+)[^\\d]*(\\d+)\\)")
      confrecord <- strsplit(favorite_header, "Record: ")[[1]][3]
      confrecord <- str_match(confrecord, "(\\d+)-(\\d+)")
      favorite$Fwins <- as.numeric(record[2])
      favorite$Flosses <- as.numeric(record[3])
      favorite$Frecordrank <- as.numeric(record[4])
      
      favorites <- rbind(favorites, favorite)  # add team to list of all favorites
      
      
      underdog_html <- read_html(temp_bowl$underdogurl[[j]])
      underdog <- html_table( 
        html_nodes(underdog_html, "table#team")[[1]] )[1:2, ]
      
      #remove some columns, rename columns, and combine to one row
      underdog <- subset(underdog, select = -c(1, 2, 13, 18, 23))
      colnames(underdog) <- c("PassComp", "PassAtt", "PassPct", "PassYds", "PassTD",
                              "RushAtt", "RushYds", "RushYdsPerAtt", "RushTD",
                              "Plays", "YdsPerPlay", 
                              "Pass1st", "Rush1st", "Penalty1st",
                              "Penalties", "PenaltyYds", 
                              "Fum", "Int")
      underdog <- cbind(underdog[1, ], underdog[2, ])
      
      for (k in 1:ncol(underdog)){
        if (k <= ncol(underdog)/2){
          colnames(underdog)[[k]] <- paste("U.O.", colnames(underdog)[[k]], sep="")
        }else{
          colnames(underdog)[[k]] <- paste("U.D.", colnames(underdog)[[k]], sep="")
        }
      }
      
      # get season record
      underdog_header <- html_nodes(underdog_html, "#meta")
      underdog_header <- html_text(underdog_header)
      record <- strsplit(underdog_header, "Record: ")[[1]][2]
      record <- str_match(record, "(\\d+)-(\\d+) \\((\\d+)[^\\d]*(\\d+)\\)")
      confrecord <- strsplit(underdog_header, "Record: ")[[1]][3]
      confrecord <- str_match(confrecord, "(\\d+)-(\\d+)")
      underdog$Uwins <- as.numeric(record[2])
      underdog$Ulosses <- as.numeric(record[3])
      underdog$Urecordrank <- as.numeric(record[4])
      
      underdogs <- rbind(underdogs, underdog)
      print(temp_bowl[j, 1])
    }
    
    games <- rbind(games, temp_bowl)
    print(bowls[i, 1])
  }
}

data <- cbind(games, favorites, underdogs)
write.csv(data, "/Users/laurenbretz/Dropbox/MSU_App_Stats/Courses/Fall2016/CSE 881/Project/projdata_2000.csv", row.names = FALSE)
