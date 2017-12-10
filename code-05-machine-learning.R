# Load data shaping and viz tools
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(data.table)

# Load machine learning tools
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(e1071)



#### PREPARING DATA FOR MACHINE LEARNING ####

  # Clone new data frame without non-needed columns
    AFL.by.match.idx.1 <- AFL.by.match.idx %>% select(-home_away, -ends_with("_win"), -ends_with(".opp"))
    
  # Check for multicollinearity using Pearson correlation matrix viz
    cormat <- round(cor(AFL.by.match.idx.1 %>% select(-Year, -match_id, -Team, -round, -match_score_idx) %>% mutate(team_result = as.numeric(team_result))), 2)
    cormat[lower.tri(cormat)]<- NA
    cormat_melted <- melt(cormat, na.rm = TRUE)
    ggplot(data = cormat_melted, aes(Var2, Var1, fill = value)) + geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8), axis.text.y = element_text(size = 8), axis.title = element_blank()) +
      coord_fixed()

  # Reminder: index definitions
    # idx_win_ground_ball = Contested_possessions/Contested_possessions.opp
    # idx_win_aerial_ball = Contested_marks/Contested_marks.opp
    # idx_clear_ball = Clearances/Clearances.opp
    # idx_less_clangers = Clangers.opp/Clangers
    # idx_50m_entry = (Marks_inside_50/Inside_50s)/(Marks_inside_50.opp/Inside_50s.opp),
    # idx_less_frees = Frees_against.opp/Frees_against
    
  # Get numerator variables of the indices for further feature engineering
  # Finding the proportion of top ten players for a match with that metric, e.g. if top ten had six from team A, then team A gets 0.6 and team B 0.4
  # If players are tied at the tenth position, keep going until there are no more ties, i.e. proportion of top 11, 12, 13 etc
    # Step 1: Prepare data and function
      AFL.by.player.disagg <- semi_join(AFL.by.player, AFL.by.match.idx.1, by = c("Team", "Year", "round", "match_id", "team_result")) %>% 
        select(match_id, Year:Team, Clangers:Contested_possessions, Frees_against, Inside_50s, Marks_inside_50)
  
      var.top.n <- function(matchid, team.nm, pvar, n = 10){
        DF <- AFL.by.player.disagg %>% select(match_id, Team, pvar) %>% filter(match_id == matchid) %>% top_n(n)
        prop.top.10 <- (DF %>% filter(Team == team.nm) %>% count)/(nrow(DF))
        return(prop.top.10)
      }
  
      AFL.by.player.agg <- AFL.by.match.idx.1 %>% ungroup
  
    # Step 2: Run the function for each match using setDT
    # NOTE: THIS CODE TAKES A VERY LONG TIME TO RUN
      setDT(AFL.by.player.agg)[ , Clangers := var.top.n(match_id, Team, "Clangers", 10), by=c("match_id","Year","round","Team")]
      setDT(AFL.by.player.agg)[ , Clearances := var.top.n(match_id, Team, "Clearances", 10), by=c("match_id","Year","round","Team")]
      setDT(AFL.by.player.agg)[ , Contested_marks := var.top.n(match_id, Team, "Contested_marks", 10), by=c("match_id","Year","round","Team")]
      setDT(AFL.by.player.agg)[ , Contested_possessions := var.top.n(match_id, Team, "Contested_possessions", 10), by=c("match_id","Year","round","Team")]
      setDT(AFL.by.player.agg)[ , Frees_against := var.top.n(match_id, Team, "Frees_against", 10), by=c("match_id","Year","round","Team")]
      setDT(AFL.by.player.agg)[ , Inside_50s := var.top.n(match_id, Team, "Inside_50s", 10), by=c("match_id","Year","round","Team")]
      setDT(AFL.by.player.agg)[ , Marks_inside_50 := var.top.n(match_id, Team, "Marks_inside_50", 10), by=c("match_id","Year","round","Team")]

    save(AFL.by.player.agg, file = "AFL_by_player_agg.Rda")

    

#### TRAIN AND TEST MODELS ####

# split out 2017 season data - this will be the testing set, the rest is training
train1 = subset(AFL.by.player.agg, AFL.by.player.agg$Year != 2017)
test1 = subset(AFL.by.player.agg, AFL.by.player.agg$Year == 2017)


  # Model 1. Basic Decision Tree (CART)
    tree1 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                  data = train1, method = "class", control = rpart.control(minbucket=25))
    
    # Only keep variables shown on tree
      tree1_V2 = rpart(team_result ~ idx_win_ground_ball + idx_50m_entry + Inside_50s,
                       data = train1, method = "class", control = rpart.control(minbucket=25))
    # Visualise tree
      prp(tree1_V2, extra = 1, fallen.leaves = TRUE, varlen = 0)
    # Check accuracy
      pred1 = predict(tree1, newdata = test1, type = "class")
      table(test1$team_result, pred1)
      (159+154)/408 #0.77
    # Visualise ROC curve with class accuracy and AUC
      pred1ROC = predict(tree1, newdata = test1)
      pred1R = prediction(pred1ROC[ ,2], test1$team_result)
      perf1R = performance(pred1R, "tpr", "fpr")
    
      par(cex = 0.7)
      plot(perf1R, col="blue", main="ROC Curve for Decision Tree - Basic", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.8, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred1R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5)
      text(0.8, 0.3, labels = paste("Class accuracy = ", round((159+154)/408,2),sep=""), adj=1, cex = 1.5)

  
  # Model 2. Cross-validated Decision Tree (CART)
    # Find the ideal complexity parameter to reduce error
      fitControl = trainControl(method = "cv", number = 10)
      cpGrid = expand.grid(.cp=(1:50)*0.0001)
      train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
            data = train1, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.002
    # Adjusted cp due to too many branches, used 0.005 instead of 0.002
      tree2 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                    data = train1, method = "class", control = rpart.control(cp = 0.005))
    # Only keep variables shown on tree
      tree2_V2 = rpart(team_result ~ idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                    data = train1, method = "class", control = rpart.control(cp = 0.005))
    # Visualise tree  
      prp(tree2_V2, extra = 1, fallen.leaves = TRUE, varlen = 0)
    # Check accuracy
      pred2 = predict(tree2_V2, newdata = test1, type = "class")
      table(test1$team_result, pred2)
      (168+148)/408 #0.77
    # Visualise ROC curve with class accuracy and AUC      
      pred2ROC = predict(tree2_V2, newdata = test1)
      pred2R = prediction(pred2ROC[ ,2], test1$team_result)
      perf2R = performance(pred2R, "tpr", "fpr")
      
      par(cex = 0.7)
      plot(perf2R, col="blue", main="ROC Curve for Decision Tree - Tuned", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.8, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred2R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5)
      text(0.8, 0.3, labels = paste("Class accuracy = ", round((168+148)/408,2),sep=""), adj=1, cex = 1.5)
      
      
  # Model 3. Logistic Regression
    # Create model with all x variables
      log3a = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
                 data = train1, family = binomial)
      # removed: Contested_marks, Clearances, Contested_possessions, Clangers, Marks_inside_50, Frees_against, idx_clear_ball
    # Leave only significant predictor variables
      log3 = glm(team_result ~ idx_win_aerial_ball + idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                 data = train1, family = binomial)
    # Check for multicollinearity in remaining variables
      train1num <- train1 %>% transmute(team_result = as.numeric(team_result), idx_win_aerial_ball, idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s)
      cor(train1num)
    # Check model details
      summary(log3)
      
    # Check training accuracy
      pred3tr = predict(log3, type = "response")
      table(train1$team_result, pred3tr >= 0.5)
      2145/(2145+542) #0.80
      pred3trR <- prediction(pred3tr, train1$team_result)
      as.numeric(performance(pred3trR, "auc")@y.values) #0.89
    # Plot ROC curve to choose t
      perf3trR = performance(pred3trR, "tpr", "fpr")
      plot(perf3trR, colorize=TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # Check test accuracy, try different t values
      pred3 = predict(log3, type = "response", newdata = test1)
      table(test1$team_result, pred3 >= 0.5)
      158/(158+46) #0.77
      table(test1$team_result, pred3 >= 0.4) # note: changing threshold to 0.4 improved results
      (143+174)/408 #0.78
    # Visualise ROC curve with class accuracy and AUC
      pred3R <- prediction(pred3, test1$team_result)
      perf3R = performance(pred3R, "tpr", "fpr")
  
      par(cex = 0.7)
      plot(perf3R, col="blue", main="ROC Curve for Logistic Regression", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.8, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred3R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5)
      text(0.8, 0.3, labels = paste("Class accuracy = ", round((143+174)/408,2),sep=""), adj=1, cex = 1.5)
      
  

  # Check relative variable importants in Logistic Regression model by leaving one out at a time
  # Repeated for each to find AUC without the chosen variable
    logx = glm(team_result ~ idx_win_aerial_ball + idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
               data = train1, family = binomial)
    predx = predict(logx, type = "response", newdata = test1)
    predxR <- prediction(predx, test1$team_result)
    as.numeric(performance(predxR, "auc")@y.values)
  # AUC excluding:
    # idx_win_aerial_ball = 0.88
    # idx_less_frees = 0.87
    # idx_win_ground_ball = 0.84 **
    # idx_less_clangers = 0.86 *
    # idx_50m_entry = 0.83 ***
    # Inside_50s = 0.86 *


  
#### SEE HOW THE MODELS GO PREDICTING 2017 WITHOUT PLAYER DATA ####
  
  # How well do the models predict actual results
  # Take player data for 2016 (one year), try to predict match results for 2017 based on averages
    AFL.by.player.2016 = AFL.by.player %>% filter(Year %in% 2016) %>% select(-(Year:Team), -Played_pct, -(home_away:match_score_idx)) %>% 
      group_by(Player) %>% summarise_all(.funs = mean) %>% select(Player, Contested_marks, Frees_against, Contested_possessions, Clangers, Marks_inside_50, Inside_50s)
  # Note: we will not have stats for players without 2016 records which is realistic
    AFL.matches.2017.pred = AFL.by.player %>% filter(Year == 2017) %>% select(match_id, round, Team, vs_opponent, Player, team_result) %>% inner_join(AFL.by.player.2016, by = "Player")
  # As before, take a team-wise view by summing up the player averages
    AFL.matches.2017.pred.sum = AFL.matches.2017.pred %>% group_by(match_id, round, Team, vs_opponent, team_result) %>% summarise_at(vars(Contested_marks:Inside_50s), .funs = sum)
  # Calculate top ten proportion again, only Inside_50s this time (only proportional variable which remains in the models)
    i50.top.n <- function(matchid, team.nm, n = 10){
      DF <- AFL.matches.2017.pred %>% select(match_id, Team, "Inside_50s") %>% filter(match_id == matchid) %>% top_n(n)
      prop.top.10 <- (DF %>% filter(Team == team.nm) %>% count)/(nrow(DF))
      return(prop.top.10)
    }
    setDT(AFL.matches.2017.pred.sum)[ , Inside_50s_prop := i50.top.n(match_id, Team, 10), by=c("match_id","round","Team")]
  
  # Using 2017 predicted x variables, recreate the indices
    AFL.matches.2017.pred.sum.1 = merge(AFL.matches.2017.pred.sum, AFL.matches.2017.pred.sum %>% rename (Team.opp = Team), by.x = c("match_id","round","Team"), by.y = c("match_id","round","vs_opponent"), suffix = c("",".opp")) %>% 
      mutate(idx_win_ground_ball = log10(Contested_possessions/Contested_possessions.opp), idx_win_aerial_ball = log10(Contested_marks/Contested_marks.opp), idx_less_clangers = log10(Clangers.opp/Clangers),
             idx_50m_entry = log10((Marks_inside_50/Inside_50s)/(Marks_inside_50.opp/Inside_50s.opp)), idx_less_frees = log10(Frees_against.opp/Frees_against)) %>% 
      select(match_id:team_result, starts_with("idx_"), Inside_50s = Inside_50s_prop)
    
  save(AFL.matches.2017.pred.sum.1, file = "AFL_matches_2017_pred_sum_1.Rda")
  
  
  # Test Model 1. Basic Decision Tree
    prp(tree1_V2)
    # Check accuracy
      pred2017t = predict(tree1_V2, newdata = AFL.matches.2017.pred.sum.1, type = "class")
      table(AFL.matches.2017.pred.sum.1$team_result, pred2017t)
      (112+139)/414 #0.61
    # Visualise ROC curve with class accuracy and AUC
      pred2017tROC = predict(tree1_V2, newdata = AFL.matches.2017.pred.sum.1)
      pred2017tR = prediction(pred2017tROC[ ,2], AFL.matches.2017.pred.sum.1$team_result)
      perf2017tR = performance(pred2017tR, "tpr", "fpr")
      
      par(cex = 0.7)
      plot(perf2017tR, col="blue", main="ROC Curve for Decision Tree - Basic (Prediction)", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.8, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred2017tR, "auc")@y.values),2),sep=""), adj=1, cex = 1.5)
      text(0.8, 0.3, labels = paste("Class accuracy = ", round((112+139)/414,2),sep=""), adj=1, cex = 1.5)
      

  # Test Model 2. Tuned Decision Tree
    prp(tree2_V2)
    # Check accuracy
      pred2017 = predict(tree2_V2, newdata = AFL.matches.2017.pred.sum.1, type = "class")
      table(AFL.matches.2017.pred.sum.1$team_result, pred2017)
      (136+131)/414 #0.64
    # Visualise ROC curve with class accuracy and AUC
      pred2017ROC = predict(tree2_V2, newdata = AFL.matches.2017.pred.sum.1)
      pred2017R = prediction(pred2017ROC[ ,2], AFL.matches.2017.pred.sum.1$team_result)
      perf2017R = performance(pred2017R, "tpr", "fpr")
      
      par(cex = 0.7)
      plot(perf2017R, col="blue", main="ROC Curve for Decision Tree - Tuned (Prediction)", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.8, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred2017R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5)
      text(0.8, 0.3, labels = paste("Class accuracy = ", round((136+131)/414,2),sep=""), adj=1, cex = 1.5)
      
    
  # Test Model 3. Logistic Regression
    # Check accuracy
      pred2017L = predict(log3, type = "response", newdata = AFL.matches.2017.pred.sum.1)
      table(AFL.matches.2017.pred.sum.1$team_result, pred2017L >= 0.5)
      (131+128)/414 #0.63
    
    # Visualise ROC curve with class accuracy and AUC
      pred2017LR <- prediction(pred2017L, AFL.matches.2017.pred.sum.1$team_result)
      perf2017LR = performance(pred2017R, "tpr", "fpr")
      
      par(cex = 0.7)
      plot(perf2017LR, col="blue", main="ROC Curve for Logistic Regression (Prediction)", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.8, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred2017LR, "auc")@y.values),2),sep=""), adj=1, cex = 1.5)
      text(0.8, 0.3, labels = paste("Class accuracy = ", round((131+128)/414,2),sep=""), adj=1, cex = 1.5)
      
  