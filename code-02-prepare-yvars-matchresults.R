# This code uses the Excel prepared afltables.com data which has match results
# It parses the results to determine winner and margin of games
# It writes the final results to an Rda file for further analysis and blending with other data sources

setwd("C:\\Users\\james.hooi\\Documents\\data analytics\\Springboard-FDS-Capstone")


#### Load required libraries ####
library(xlsx)
library(dplyr)
library(data.table)



#### Functions ####
parse_score_str <- function(in.score.str){
  # remove white space and brackets
  in.score.str <- trimws(in.score.str, "both")
  in.score.str <- sub("\\(([0-9]+)\\.([0-9]+)\\)", "\\1\\.\\2", in.score.str)
  # split into quarter scores by space
  list.score.str <- strsplit(in.score.str, split = "\\s+")[[1]]
  return.str.cumul <- c()
  # loop through each score
  for (Qs in list.score.str){
    # turn score into a number, e.g. 4.2 (4 goals, 2 behinds)
    Qsnum_goals <- as.integer(sub("\\..*$", "", Qs))
    Qsnum_behinds <- as.integer(sub("^.*\\.", "", Qs))
    # add up scores by multiplying goals by 6 and behinds by 1
    Qsttl <- Qsnum_goals*6 + Qsnum_behinds*1
    # return string of cumulative scores
    return.str.cumul <- paste(return.str.cumul, Qsttl)
  }
  #return(trimws(return.str.cumul, "left"))
  return(trimws(return.str.cumul,"left"))
}



#### Load Excel file and extract team data ####
raw.team.res <- read.xlsx2(file = ".\\match-results-prep.xlsx", sheetName = "r_version", stringsAsFactors = FALSE)
raw.team.res$final_score <- as.integer(raw.team.res$final_score)

# Create a "match id" since every two rows is one match result
prep.team.res <- cbind(raw.team.res, match_id = rep(seq(1001, 1000+nrow(raw.team.res)/2, by=1), each=2)) %>% 
  # add basic row number
  mutate(rownum = row_number()) %>% 
  # group by match and add vs_opponent, result, margin
  group_by(match_id) %>% 
  mutate(vs_opponent = if_else(rownum == min(rownum),
                               lead(teams, 1L),
                               lag(teams, 1L))
         , team_result = if_else(final_score == max(final_score) & final_score == min(final_score),
                                 "draw",
                                 if_else(final_score == max(final_score), "win", "loss")
                                 )
         , final_margin = if_else(rownum == min(rownum),
                                  final_score - lead(final_score, 1L),
                                  final_score - lag(final_score, 1L))
         ) %>% 
  ungroup %>% 
  # remove unnecessary match summary and change score string to actual scores
  select(-match_summ) %>%
  rowwise %>% mutate(score_str = parse_score_str(score_str)) %>% 
  ungroup



#### Add detailed score columns ####
  # get list of the score strings
  score.str.list <- lapply(prep.team.res$score_str, function (x) strsplit(as.character(x), split="\\s")[[1]])
  # prepare columns for the cumulative quarter scores
  prep.team.res$Q1score_cumul <- 0
  prep.team.res$Q2score_cumul <- 0
  prep.team.res$Q3score_cumul <- 0
  prep.team.res$Q4score_cumul <- 0
  prep.team.res$ETscore_cumul <- 0
  # extract the quarter scores from the list, add to the data frame
  # adjust for the "fifth quarter" (extra time)
  for(i in 1:nrow(prep.team.res)){
    prep.team.res$Q1score_cumul[i] = as.integer(score.str.list[[i]][1])
    prep.team.res$Q2score_cumul[i] = as.integer(score.str.list[[i]][2])
    prep.team.res$Q3score_cumul[i] = as.integer(score.str.list[[i]][3])
    prep.team.res$Q4score_cumul[i] = as.integer(score.str.list[[i]][4])
    prep.team.res$ETscore_cumul[i] = if_else(!is.na(score.str.list[[i]][5]), as.integer(score.str.list[[i]][5]), NULL)
  }
  # calculate the non-cumulative quarter scores and add to the data frame
  prep.team.res <- prep.team.res %>% 
    mutate(Q1score = Q1score_cumul
           , Q2score = Q2score_cumul - Q1score_cumul
           , Q3score = Q3score_cumul - Q2score_cumul
           , Q4score = Q4score_cumul - Q3score_cumul
           , ETscore = ETscore_cumul - Q4score_cumul) %>% 
    group_by(match_id) %>% 
    # add a flag to indicate whether the team won the quarter
    # drawn quarter is won by no one so is 0 for both teams
    mutate(Q1_win = if_else(Q1score == min(Q1score), 0, 1)
           , Q2_win = if_else(Q2score == min(Q2score), 0, 1)
           , Q3_win = if_else(Q3score == min(Q3score), 0, 1)
           , Q4_win = if_else(Q4score == min(Q4score), 0, 1)
           , ET_win = if_else(ETscore == min(ETscore), 0, 1)
           )
  

  
#### Final wrangling to get terms and formats consistent with player data ####  
  # adjustments to round
  prep.team.res$round <- sub("Round\\s(.*)$", "R\\1", prep.team.res$round)
  prep.team.res[which(prep.team.res$round=="Elimination Final"), "round"] <- "EF"
  prep.team.res[which(prep.team.res$round=="Qualifying Final"), "round"] <- "QF"
  prep.team.res[which(prep.team.res$round=="Semi Final"), "round"] <- "SF"
  prep.team.res[which(prep.team.res$round=="Preliminary Final"), "round"] <- "PF"
  prep.team.res[which(prep.team.res$round=="Grand Final"), "round"] <- "GF"
  # SPECIAL CASE: St Kilda v Collingwood, 2010, grand final replay
  prep.team.res[which(prep.team.res$match_id==2623 &
                        prep.team.res$teams %in% c("St Kilda","Collingwood") &
                        prep.team.res$season == "2010" &
                        prep.team.res$round == "GF"), "round"] <- "GF.1"
  
  # adjustments to data type
  prep.team.res$season <- as.numeric(prep.team.res$season)
  
  # column rename
  prep.team.res <- prep.team.res %>% 
    rename(Year = season, Team = teams)
  
  
  
#### Save to Rda file for loading to analysis ####  
  save(prep.team.res, file = ".\\teamresults_1998to2017.Rda")
  
  