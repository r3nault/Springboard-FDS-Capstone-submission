library(dplyr)

# Load indices
load(file=".\\afl_by_match_idx.Rda")


# Statistical test 1: Re-randomization
  # Randomly assign win/loss results to index data
  # Create re-randomised results 1000 times
  AFL.by.match.idx.random <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% mutate(team_result = sample(0:1, nrow(AFL.by.match.idx.random), replace = TRUE))
  AFL.by.match.idx.random.res <- AFL.by.match.idx.random %>% group_by(team_result) %>%  summarise_all(funs(mean))
  
  for(i in 2:1000){
    AFL.by.match.idx.random$team_result <- sample(0:1, nrow(AFL.by.match.idx.random), replace = TRUE)
    AFL.by.match.idx.random.res <- rbind(AFL.by.match.idx.random.res, AFL.by.match.idx.random %>% group_by(team_result) %>% summarise_all(funs(mean)))
  }
  
  # Example index - repeat for all indices
  # Check how many of 1000 re-randomised index means are greater than mean for wins in original data
  Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_win_aerial_ball))
  print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_win_ground_ball <= Xbar[[2]][[1]] | idx_win_ground_ball >= Xbar[[2]][[2]])) %>% count()))

  # Save re-randomised data so results are repeatable  
  save(AFL.by.match.idx.random.res, file = ".\\AFL_by_match_idx_random_res.Rda")


# Statistical test 2: Significance test
  # Calculate mean and sd for each index from population
  # Null hypothesis is the true mean of an index is zero since it has no influence on the match result, and the probability of winning is 0.5 (random)
  # Get z-score of each index and find probability of getting that score
  
    #idx_win_ground_ball
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_win_ground_ball))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_win_ground_ball <= Xbar[[2]][[1]] | idx_win_ground_ball >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_win_ground_ball), sd(idx_win_ground_ball), n())))
    (0.02814632-0)/0.05430982 = 0.52
    
    #idx_win_aerial_ball
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_win_aerial_ball))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_win_aerial_ball <= Xbar[[2]][[1]] | idx_win_aerial_ball >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_win_aerial_ball), sd(idx_win_aerial_ball), n())))
    (0.07908668-0)/0.2371167 = 0.33
    
    #idx_clear_ball
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_clear_ball))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_clear_ball <= Xbar[[2]][[1]] | idx_clear_ball >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_clear_ball), sd(idx_clear_ball), n())))
    (0.03018063-0)/0.1118536 = 0.27
    
    #idx_less_clangers
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_less_clangers))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_less_clangers <= Xbar[[2]][[1]] | idx_less_clangers >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_less_clangers), sd(idx_less_clangers), n())))
    (0.02599295-0)/0.08638041 = 0.30
    
    #idx_goal_assist
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_goal_assist))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_goal_assist <= Xbar[[2]][[1]] | idx_goal_assist >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_goal_assist), sd(idx_goal_assist), n())))
    (0.01951481-0)/0.1622946 = 0.12
    
    #idx_mark_kick
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_mark_kick))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_mark_kick <= Xbar[[2]][[1]] | idx_mark_kick >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_mark_kick), sd(idx_mark_kick), n())))
    (0.01494406-0)/0.07863325 = 0.19
    
    #idx_50m_entry
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_50m_entry))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_50m_entry <= Xbar[[2]][[1]] | idx_50m_entry >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_50m_entry), sd(idx_50m_entry), n())))
    (0.09124001-0)/0.2041845 = 0.45
    
    #idx_tackle
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_tackle))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_tackle <= Xbar[[2]][[1]] | idx_tackle >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_tackle), sd(idx_tackle), n())))
    (0.01553383-0)/0.09970744 = 0.16
    
    #idx_one_pct
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_one_pct))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_one_pct <= Xbar[[2]][[1]] | idx_one_pct >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_one_pct), sd(idx_one_pct), n())))
    (0.01087388-0)/0.1073182 = 0.10
    
    #idx_less_frees
    Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_less_frees))
    print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_less_frees <= Xbar[[2]][[1]] | idx_less_frees >= Xbar[[2]][[2]])) %>% count()))
    print(as.data.frame(AFL.by.match.idx %>% group_by(team_result) %>% summarise(mean(idx_less_frees), sd(idx_less_frees), n())))
    (0.008991437-0)/0.1516608 = 0.06










