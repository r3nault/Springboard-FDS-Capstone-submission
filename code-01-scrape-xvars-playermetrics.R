# This code accesses the afltables.com site and uses html tags to scrape data
# It goes through the in-scope teams and years to get all player stats for all games
# It writes the final results to an Rda file for further analysis and blending with other data sources

setwd("C:\\Users\\james.hooi\\Documents\\data analytics\\Springboard-FDS-Capstone")


#### CONSTANTS ####
  # General time limits to analysis
  start.yr <- 1998
  end.yr <- 2005
  # Team aliases and start dates (allowing for teams with shorter histories)
    # Alias to be used in the analysis
    current.teams.alias <- c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",
                             "Geelong","Gold Coast","Greater Western Sydney","Hawthorn","Melbourne","North Melbourne",
                             "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")
    # Alias for generating URLs to scrape data
    current.teams.url <- c("adelaide","brisbanel","carlton","collingwood","essendon","fremantle",
                             "geelong","goldcoast","gws","hawthorn","melbourne","kangaroos",
                             "padelaide","richmond","stkilda","swans","westcoast","bullldogs")
    # Start years for each team
    current.teams.start.yr <- c(1991,1997,1897,1897,1897,1995,
                           1897,2011,2012,1925,1897,1925,
                           1997,1908,1897,1897,1987,1925)
    # Round labels denoting finals games
    finals.rounds <- c("QF","EF","SF","PF","GF")


#### Load required libraries ####
library(rvest)
library(dplyr)

    

#### Functions ####
  # URL generator
  player.scraper.url <- function(in.team, in.year){
    return(paste0("https://afltables.com/afl/stats/teams/",
             in.team,
             "/",
             in.year,
             "_gbg.html"))
  }
  
  # Open generated URL, extract data from html, return data frame
  player.scraper.exec <- function(in.url, in.alias, in.year){
    # Read html source from supplied URL
    raw.html <- read_html(in.url)
    # Extract relevant tags into vectors
    data.headers <- raw.html %>% html_nodes(xpath="//thead//th") %>% html_text()
    data.body <- raw.html %>% html_nodes(xpath="//tbody//td") %>% html_text()
    
    # 1. Wrangle headers to understand the width of the data
      # Extract all the metrics headers by removing specific strings (Player, Round, Tot headings)
      # e.g. Disposals, Kicks, etc
      stats.labels <- data.headers[!(
        grepl(pattern = "Player", data.headers) |
          grepl(pattern = "R[0-9]+", data.headers) |
          data.headers %in% c(finals.rounds,"Tot")
      )]
      # Removing these values will leave just the player and round headings
      # e.g. Player, R1, R2 etc for each game played
      stats.labels.rounds <- data.headers[!(data.headers %in% stats.labels)]
      # Get number of rounds the team played, this tells us how many columns to create in the data frame
      # as above but only once
      stats.rounds <- stats.labels.rounds[1:(length(stats.labels.rounds)/length(stats.labels))]
      
    # 2. Wrangle the actual metrics from the body of the data
      # Create data frame with correct number of columns for number of rounds played
      sync.df <- as.data.frame(matrix(data.body, ncol=length(stats.rounds), byrow = TRUE), stringsAsFactors = FALSE)
      colnames(sync.df) <- stats.rounds
    
    # 3. Add information to identify the rows later
      # Get the player names from the body, identifiable by the presence of a comma-space sequence in the string
      # e.g. Bews, Jed
      team.players <- unique(data.body[grepl(pattern = "*,\\s*", data.body)])
      # Add column with the metric being tracked
      # e.g. Disposals, Kicks, etc repeated for each player, for each round
      sync.df[,"Metric"] <- rep(stats.labels, each=length(team.players))
      # Add columns for team and year
      # e.g. Geelong, 2017
      sync.df[,"Team"] <- rep(in.alias, times=nrow(sync.df))
      sync.df[,"Year"] <- rep(in.year, times=nrow(sync.df))
    
    # Take a nap (R dislikes running html scraping in a loop too rapidly!)
    Sys.sleep(5)
      
    # 4. Return the prepared data frame to function call
    return(sync.df)
  }
  
  # Simplify function call for LAPPLY
  player.scraper.exec.do <- function(i){
    # Print a line so we know where we're up to
    print(paste(Sys.time(), "Extracting player data for", par.alias[i], par.years[i]))
    # Call the URL extraction
    inner.df <- player.scraper.exec(player.scraper.url(par.teams[i], par.years[i]),
                                    par.alias[i],
                                    par.years[i])
    # Return the generated data frame
    return(inner.df)
  }
    
    
    
#### Prepare for extraction ####
  # variables to hold URL generator parameters (team and year)
  par.years <- c()
  par.teams <- c()
  par.alias <- c()
  # for each year in scope
  for(y in seq(start.yr, end.yr, by=1)){
    # for each team, check if team had started by that year
    for(t in 1:length(current.teams.url)){
      if(current.teams.start.yr[t] <= y){
        par.years <- c(par.years, y)
        par.teams <- c(par.teams, current.teams.url[t])
        par.alias <- c(par.alias, current.teams.alias[t])
      }
    }
  }
  # end result is two equal length vectors with each year and each team to feed to URL generator


  
#### Load web pages and extract player data ####
  # create sequencing variable
  x <- seq_along(par.teams)
  
  # extract to list of data frames
  list.df <- lapply(x, FUN = player.scraper.exec.do)
  
  # consolidate in one data frame
    # prepare master data frame with all required columns
    # up to 24 rounds plus finals plus totals
    cons.headers = c("Player"
                     , paste0("R",seq(1,24,by=1)), finals.rounds, "Tot"
                     , "Metric", "Team", "Year")
    cons.df <- as.data.frame(matrix(ncol = length(cons.headers), nrow = 0), stringsAsFactors = FALSE)
    colnames(cons.df) <- cons.headers
    
    # populate data from list of data frames
    for(i in x){
      cons.df <- union_all(cons.df, list.df[[i]])
    }
    
  
  
    
#### Once-off extraction of data to Rda file ####  
  # extraction done six years at a time to ensure it works correctly
  # each six year block exported to Rda file to reload later
  
  # save(cons.df, file = paste(getwd(), "playerstats_2012to2017.Rda", sep="\\"))
  # save(cons.df, file = paste(getwd(), "playerstats_2006to2011.Rda", sep="\\"))
  # save(cons.df, file = paste(getwd(), "playerstats_1998to2005.Rda", sep="\\"))
  
    




  
  
## working example - test code
# 
# 
#   rawext <- read_html("https://afltables.com/afl/stats/teams/brisbanel/2017_gbg.html")
#   raw.head <- rawext %>% html_nodes(xpath="//thead//th") %>% html_text()
#   raw.body <- rawext %>% html_nodes(xpath="//tbody//td") %>% html_text()
# 
# 
#   # Take header stat labels
#   stats.labels <- raw.head[!(
#     grepl(pattern = "Player", raw.head) |
#       grepl(pattern = "R[0-9]+", raw.head) |
#       raw.head %in% c("QF","EF","SF","PF","GF","Tot")
#   )]
#   stats.values <- raw.head[!(raw.head %in% stats.labels)]
# 
#   # Get number of rounds the team played
#   team.rounds <- stats.values[1:(length(stats.values)/length(stats.labels))]
# 
#   # Create data frame of all stats
#   team.df <- as.data.frame(matrix(raw.body, ncol=length(team.rounds), byrow = TRUE), stringsAsFactors = FALSE)
#   colnames(team.df) <- team.rounds
# 
#   # Add column with the actual stat listed
#   # Get number of players listed
#   team.players <- unique(raw.body[grepl(pattern = "*,\\s*", raw.body)])
#   # Repeat stat labels by number of players
#   team.df[,"Metric"] <- rep(stats.labels, each=length(team.players))
#   # Repeat team name for all rows
#   team.df[,"Team"] <- rep("Brisbane", times=nrow(team.df))