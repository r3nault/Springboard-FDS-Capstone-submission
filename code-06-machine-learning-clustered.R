library(dplyr)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(e1071)

library(flexclust)


#### CREATE CLUSTERS - TRAINING AND TEST ####
  # Convert data to matrices
    train1_Matrix = as.matrix(train1 %>% select(idx_win_ground_ball, idx_win_aerial_ball, idx_less_clangers, idx_50m_entry, idx_less_frees, Inside_50s))
    test1_Matrix = as.matrix(test1 %>% select(idx_win_ground_ball, idx_win_aerial_ball, idx_less_clangers, idx_50m_entry, idx_less_frees, Inside_50s))
    AFL.matches.2017.pred.sum.1_Matrix = as.matrix(AFL.matches.2017.pred.sum.1 %>% select(idx_win_ground_ball:Inside_50s))

  # Assume 3x2=6 clusters (team: top, middle, bottom; strategy: fast, slow)
    k=6
    set.seed(1)

  # Perform k-means clustering
    train1_kmc = kmeans(train1_Matrix, centers = k, iter.max = 10000)
    train1_clusters = train1_kmc$cluster

  # Assign clusters to data sets from the training clustering
    train1_kcca = as.kcca(train1_kmc, train1_Matrix)
    test1_clusters = predict(train1_kcca, newdata = test1_Matrix)
    AFL.matches.2017.pred.sum.1_clusters = predict(train1_kcca, newdata = AFL.matches.2017.pred.sum.1_Matrix)

  # Get subsets of data per cluster
    train1_clust1 = subset(train1, train1_clusters==1)  
    train1_clust2 = subset(train1, train1_clusters==2)    
    train1_clust3 = subset(train1, train1_clusters==3)  
    train1_clust4 = subset(train1, train1_clusters==4)    
    train1_clust5 = subset(train1, train1_clusters==5)  
    train1_clust6 = subset(train1, train1_clusters==6)   

    test1_clust1 = subset(test1, test1_clusters==1)  
    test1_clust2 = subset(test1, test1_clusters==2)    
    test1_clust3 = subset(test1, test1_clusters==3)  
    test1_clust4 = subset(test1, test1_clusters==4)    
    test1_clust5 = subset(test1, test1_clusters==5)  
    test1_clust6 = subset(test1, test1_clusters==6)

    pred2017_clust1 = subset(AFL.matches.2017.pred.sum.1, AFL.matches.2017.pred.sum.1_clusters==1)
    pred2017_clust2 = subset(AFL.matches.2017.pred.sum.1, AFL.matches.2017.pred.sum.1_clusters==2)
    pred2017_clust3 = subset(AFL.matches.2017.pred.sum.1, AFL.matches.2017.pred.sum.1_clusters==3)
    pred2017_clust4 = subset(AFL.matches.2017.pred.sum.1, AFL.matches.2017.pred.sum.1_clusters==4)
    pred2017_clust5 = subset(AFL.matches.2017.pred.sum.1, AFL.matches.2017.pred.sum.1_clusters==5)
    pred2017_clust6 = subset(AFL.matches.2017.pred.sum.1, AFL.matches.2017.pred.sum.1_clusters==6)


#### BUILD DECISION TREES - one for each cluster ####

  # cluster 1
    # tuning
      fitControl = trainControl(method = "cv", number = 10)
      cpGrid = expand.grid(.cp=(1:50)*0.001)
      train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
            data = train1_clust1, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.05
    # build
      tree_clust1 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                          data = train1_clust1, method = "class", control = rpart.control(cp = 0.02)) #altered cp
      prp(tree_clust1, extra = 1, fallen.leaves = TRUE, varlen = 0)  
    # test
      table(test1_clust1$team_result, predict(tree_clust1, newdata = test1_clust1, type = "prob")[,2] > 0.5)
      46/51 #0.90
      pred_tree_clust1 = prediction(predict(tree_clust1, newdata = test1_clust1)[ ,2], test1_clust1$team_result) # note: changing threshold did not improve results
      perf_tree_clust1 = performance(pred_tree_clust1, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_tree_clust1, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust1, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.87
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(46/51,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 2
    # tuning
      fitControl = trainControl(method = "cv", number = 10)
      cpGrid = expand.grid(.cp=(1:50)*0.001)
      train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
            data = train1_clust2, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.034
    # build
      tree_clust2 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                          data = train1_clust2, method = "class", control = rpart.control(cp = 0.034))
      prp(tree_clust2, extra = 1, fallen.leaves = TRUE, varlen = 0)  
    # test
      table(test1_clust2$team_result, predict(tree_clust2, newdata = test1_clust2, type = "prob")[,2] > 0.5)
      55/66 #0.83
      pred_tree_clust2 = prediction(predict(tree_clust2, newdata = test1_clust2)[ ,2], test1_clust2$team_result) # note: changing threshold did not improve results
      perf_tree_clust2 = performance(pred_tree_clust2, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_tree_clust2, col="blue", main="ROC Curve for Decision Tree - Cluster 2", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust2, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.59
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(55/66,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 3
    # tuning
      fitControl = trainControl(method = "cv", number = 10)
      cpGrid = expand.grid(.cp=(1:50)*0.0001)
      train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
            data = train1_clust3, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.005
    # build
      tree_clust3 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                          data = train1_clust3, method = "class", control = rpart.control(cp = 0.02)) #altered cp
      prp(tree_clust3, extra = 1, fallen.leaves = TRUE, varlen = 0)  
    # test
      table(test1_clust3$team_result, predict(tree_clust3, newdata = test1_clust3, type = "prob")[,2] > 0.5)
      49/61 #0.80
      pred_tree_clust3 = prediction(predict(tree_clust3, newdata = test1_clust3)[ ,2], test1_clust3$team_result) # note: changing threshold did not improve results
      perf_tree_clust3 = performance(pred_tree_clust3, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_tree_clust3, col="blue", main="ROC Curve for Decision Tree - Cluster 3", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust3, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.83
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(49/61,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 4
    # tuning
      fitControl = trainControl(method = "cv", number = 10)
      cpGrid = expand.grid(.cp=(1:50)*0.001)
      train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
            data = train1_clust4, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.007
    # build
      tree_clust4 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                          data = train1_clust4, method = "class", control = rpart.control(cp = 0.01)) #altered cp
      prp(tree_clust4, extra = 1, fallen.leaves = TRUE, varlen = 0)  
    # test
      table(test1_clust4$team_result, predict(tree_clust4, newdata = test1_clust4, type = "prob")[,2] > 0.5)
      63/83 #0.76
      pred_tree_clust4 = prediction(predict(tree_clust4, newdata = test1_clust4)[ ,2], test1_clust4$team_result) # note: changing threshold did not improve results
      perf_tree_clust4 = performance(pred_tree_clust4, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_tree_clust4, col="blue", main="ROC Curve for Decision Tree - Cluster 4", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust4, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.78
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(63/83,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 5
    # tuning
      fitControl = trainControl(method = "cv", number = 10)
      cpGrid = expand.grid(.cp=(1:50)*0.001)
      train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
            data = train1_clust5, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.005
    # build
      tree_clust5 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                          data = train1_clust5, method = "class", control = rpart.control(cp = 0.019)) #altered cp
      prp(tree_clust5, extra = 1, fallen.leaves = TRUE, varlen = 0)  
    # test
      table(test1_clust5$team_result, predict(tree_clust5, newdata = test1_clust5, type = "prob")[,2] > 0.5)
      62/88 #0.69
      pred_tree_clust5 = prediction(predict(tree_clust5, newdata = test1_clust5)[ ,2], test1_clust5$team_result) # note: changing threshold to 0.7 makes sense here
      perf_tree_clust5 = performance(pred_tree_clust5, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_tree_clust5, col="blue", main="ROC Curve for Decision Tree - Cluster 5", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust5, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.65
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(62/88,2)*100, "%", sep=""), adj=1, cex = 1.5)
    
  # cluster 6
    # tuning
      fitControl = trainControl(method = "cv", number = 10)
      cpGrid = expand.grid(.cp=(1:50)*0.001)
      train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
            data = train1_clust6, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.038
    # build
      tree_clust6 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                          data = train1_clust6, method = "class", control = rpart.control(cp = 0.038))
      prp(tree_clust6, extra = 1, fallen.leaves = TRUE, varlen = 0)  
    # test
      table(test1_clust6$team_result, predict(tree_clust6, newdata = test1_clust6, type = "prob")[,2] > 0.5)
      50/59 #0.85
      pred_tree_clust6 = prediction(predict(tree_clust6, newdata = test1_clust6)[ ,2], test1_clust6$team_result) # note: changing threshold did not improve results
      perf_tree_clust6 = performance(pred_tree_clust6, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_tree_clust6, col="blue", main="ROC Curve for Decision Tree - Cluster 6", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust6, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.76
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(44/52,2)*100, "%", sep=""), adj=1, cex = 1.5)  


#### BUILD LOGISTIC REGRESSIONS - one for each cluster ####
  # cluster 1
      log_clustx = glm(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust1, family = binomial)
      summary(log_clustx)
    # remove variables one by one until all significant, then check multicollinearity
      log_clust1 = glm(team_result ~ idx_win_ground_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust1, family = binomial)
      summary(log_clust1)
      cor(train1_clust1 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
      pred_log_train1clust1 = predict(log_clust1, type = "response")
      pred_log_train1clust1R = prediction(pred_log_train1clust1, as.factor(train1_clust1$team_result))
      perf_log_train1clust1R = performance(pred_log_train1clust1R, "tpr", "fpr")
      plot(perf_log_train1clust1R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # test accuracy
      pred_log_clust1 = predict(log_clust1, type = "response", newdata = test1_clust1)
      table(test1_clust1$team_result, pred_log_clust1 >= 0.2)
      46/51 #0.90
      pred_log_clust1R = prediction(pred_log_clust1, as.factor(test1_clust1$team_result))
      # plot ROC curve
      perf_log_clust1R = performance(pred_log_clust1R, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_log_clust1R, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust1R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.93
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(46/51,2)*100, "%", sep=""), adj=1, cex = 1.5)  

  # cluster 2
      log_clustx = glm(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust2, family = binomial)
      log_clust2 = glm(team_result ~ idx_win_ground_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust2, family = binomial)
      summary(log_clust2)
      cor(train1_clust2 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
      pred_log_train1clust2 = predict(log_clust2, type = "response")
      pred_log_train1clust2R = prediction(pred_log_train1clust2, as.factor(train1_clust2$team_result))
      perf_log_train1clust2R = performance(pred_log_train1clust2R, "tpr", "fpr")
      plot(perf_log_train1clust2R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # test accuracy
      pred_log_clust2 = predict(log_clust2, type = "response", newdata = test1_clust2)
      table(test1_clust2$team_result, pred_log_clust2 >= 0.7)
      55/66 #0.83
      pred_log_clust2R = prediction(pred_log_clust2, as.factor(test1_clust2$team_result))
      # plot ROC curve
      perf_log_clust2R = performance(pred_log_clust2R, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_log_clust2R, col="blue", main="ROC Curve for Logistic Regression - Cluster 2", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust2R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.89
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(55/66,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 3
      log_clustx = glm(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust3, family = binomial)
      log_clust3 = glm(team_result ~ idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                       data = train1_clust3, family = binomial)
      summary(log_clust3)
      cor(train1_clust3 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
      pred_log_train1clust3 = predict(log_clust3, type = "response")
      pred_log_train1clust3R = prediction(pred_log_train1clust3, as.factor(train1_clust3$team_result))
      perf_log_train1clust3R = performance(pred_log_train1clust3R, "tpr", "fpr")
      plot(perf_log_train1clust3R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # test accuracy
      pred_log_clust3 = predict(log_clust3, type = "response", newdata = test1_clust3)
      table(test1_clust3$team_result, pred_log_clust3 >= 0.4)
      46/61 #0.75
      pred_log_clust3R = prediction(pred_log_clust3, as.factor(test1_clust3$team_result))
      # plot ROC curve
      perf_log_clust3R = performance(pred_log_clust3R, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_log_clust3R, col="blue", main="ROC Curve for Logistic Regression - Cluster 3", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust3R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.84
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(46/61,2)*100, "%", sep=""), adj=1, cex = 1.5)      

  # cluster 4
      log_clustx = glm(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust4, family = binomial)
      log_clust4 = glm(team_result ~ idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                       data = train1_clust4, family = binomial)
      summary(log_clust4)
      cor(train1_clust4 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
      pred_log_train1clust4 = predict(log_clust4, type = "response")
      pred_log_train1clust4R = prediction(pred_log_train1clust4, as.factor(train1_clust4$team_result))
      perf_log_train1clust4R = performance(pred_log_train1clust4R, "tpr", "fpr")
      plot(perf_log_train1clust4R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # test accuracy
      pred_log_clust4 = predict(log_clust4, type = "response", newdata = test1_clust4)
      table(test1_clust4$team_result, pred_log_clust4 >= 0.4)
      62/83 #0.75
      pred_log_clust4R = prediction(pred_log_clust4, as.factor(test1_clust4$team_result))
      # plot ROC curve
      perf_log_clust4R = performance(pred_log_clust4R, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_log_clust4R, col="blue", main="ROC Curve for Logistic Regression - Cluster 4", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust4R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.81
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(62/83,2)*100, "%", sep=""), adj=1, cex = 1.5)      

  # cluster 5
      log_clustx = glm(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust5, family = binomial)
      log_clust5 = glm(team_result ~ idx_win_ground_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust5, family = binomial)
      summary(log_clust5)
      cor(train1_clust5 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
      pred_log_train1clust5 = predict(log_clust5, type = "response")
      pred_log_train1clust5R = prediction(pred_log_train1clust5, as.factor(train1_clust5$team_result))
      perf_log_train1clust5R = performance(pred_log_train1clust5R, "tpr", "fpr")
      plot(perf_log_train1clust5R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # test accuracy
      pred_log_clust5 = predict(log_clust5, type = "response", newdata = test1_clust5)
      table(test1_clust5$team_result, pred_log_clust5 >= 0.7)
      64/88 #0.73
      pred_log_clust5R = prediction(pred_log_clust5, as.factor(test1_clust5$team_result))
      # plot ROC curve
      perf_log_clust5R = performance(pred_log_clust5R, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_log_clust5R, col="blue", main="ROC Curve for Logistic Regression - Cluster 5", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust5R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.72
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(64/88,2)*100, "%", sep=""), adj=1, cex = 1.5)      

  # cluster 6
      log_clustx = glm(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust6, family = binomial)
      log_clust6 = glm(team_result ~ idx_win_ground_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Inside_50s,
                       data = train1_clust6, family = binomial)
      summary(log_clust6)
      cor(train1_clust6 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
      pred_log_train1clust6 = predict(log_clust6, type = "response")
      pred_log_train1clust6R = prediction(pred_log_train1clust6, as.factor(train1_clust6$team_result))
      perf_log_train1clust6R = performance(pred_log_train1clust6R, "tpr", "fpr")
      plot(perf_log_train1clust6R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # test accuracy
      pred_log_clust6 = predict(log_clust6, type = "response", newdata = test1_clust6)
      table(test1_clust6$team_result, pred_log_clust6 >= 0.5)
      50/59 #0.85
      pred_log_clust6R = prediction(pred_log_clust6, as.factor(test1_clust6$team_result))
      # plot ROC curve
      perf_log_clust6R = performance(pred_log_clust6R, "tpr", "fpr")
        par(cex = 0.6)
        plot(perf_log_clust6R, col="blue", main="ROC Curve for Logistic Regression - Cluster 6", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
        text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust6R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.84
        text(0.9, 0.22, labels = paste("Class accuracy = ", round(50/59,2)*100, "%", sep=""), adj=1, cex = 1.5)  
      
      
      
#### PREDICT 2017 RESULTS - player data redacted ####
  
  # DECISION TREE
        
    # cluster 1 example for write up
      table(pred2017_clust1$team_result, predict(tree_clust1, newdata = pred2017_clust1, type = "prob")[,2] > 0.5)
      pred_tree_clust1_2017 = prediction(predict(tree_clust1, newdata = pred2017_clust1)[ ,2], pred2017_clust1$team_result)
      as.numeric(performance(pred_tree_clust1_2017, "auc")@y.values)
      
    # cluster 1
      table(pred2017_clust1$team_result, predict(tree_clust1, newdata = pred2017_clust1, type = "prob")[,2] > 0.5)
      11/11 #1.00
      pred_tree_clust1_2017 = prediction(predict(tree_clust1, newdata = pred2017_clust1)[ ,2], pred2017_clust1$team_result)
      perf_tree_clust1_2017 = performance(pred_tree_clust1_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_tree_clust1_2017, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust1_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #1.00
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(11/11,2)*100, "%", sep=""), adj=1, cex = 1.5)
      
    # cluster 2
      table(pred2017_clust2$team_result, predict(tree_clust2, newdata = pred2017_clust2, type = "prob")[,2] > 0.5)
      11/17 #0.65
      pred_tree_clust2_2017 = prediction(predict(tree_clust2, newdata = pred2017_clust2)[ ,2], pred2017_clust2$team_result)
      perf_tree_clust2_2017 = performance(pred_tree_clust2_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_tree_clust2_2017, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust2_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.50
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(11/17,2)*100, "%", sep=""), adj=1, cex = 1.5)
      
    # cluster 3
      table(pred2017_clust3$team_result, predict(tree_clust3, newdata = pred2017_clust3, type = "prob")[,2] > 0.5)
      29/43 #0.67
      pred_tree_clust3_2017 = prediction(predict(tree_clust3, newdata = pred2017_clust3)[ ,2], pred2017_clust3$team_result)
      perf_tree_clust3_2017 = performance(pred_tree_clust3_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_tree_clust3_2017, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust3_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.65
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(29/43,2)*100, "%", sep=""), adj=1, cex = 1.5)
      
    # cluster 4
      table(pred2017_clust4$team_result, predict(tree_clust4, newdata = pred2017_clust4, type = "prob")[,2] > 0.5)
      86/133 #0.65
      pred_tree_clust4_2017 = prediction(predict(tree_clust4, newdata = pred2017_clust4)[ ,2], pred2017_clust4$team_result)
      perf_tree_clust4_2017 = performance(pred_tree_clust4_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_tree_clust4_2017, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust4_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.65
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(86/133,2)*100, "%", sep=""), adj=1, cex = 1.5)
      
    # cluster 5
      table(pred2017_clust5$team_result, predict(tree_clust5, newdata = pred2017_clust5, type = "prob")[,2] > 0.5)
      94/146 #0.64
      pred_tree_clust5_2017 = prediction(predict(tree_clust5, newdata = pred2017_clust5)[ ,2], pred2017_clust5$team_result)
      perf_tree_clust5_2017 = performance(pred_tree_clust5_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_tree_clust5_2017, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust5_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.63
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(94/146,2)*100, "%", sep=""), adj=1, cex = 1.5)
    
    # cluster 6
      table(pred2017_clust6$team_result, predict(tree_clust6, newdata = pred2017_clust6, type = "prob")[,2] > 0.5)
      39/64 #0.61
      pred_tree_clust6_2017 = prediction(predict(tree_clust6, newdata = pred2017_clust6)[ ,2], pred2017_clust6$team_result)
      perf_tree_clust6_2017 = performance(pred_tree_clust6_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_tree_clust6_2017, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust6_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.66
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(42/64,2)*100, "%", sep=""), adj=1, cex = 1.5)
      
      
  # LOGISTIC REGRESSION
      
    # cluster 1
      pred_log_clust1_2017 = predict(log_clust1, type = "response", newdata = pred2017_clust1)
      table(pred2017_clust1$team_result, pred_log_clust1_2017 >= 0.5)
      10/11 #0.91
      pred_log_clust1R_2017 = prediction(pred_log_clust1_2017, as.factor(pred2017_clust1$team_result))
      # plot ROC curve
      perf_log_clust1R_2017 = performance(pred_log_clust1R_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust1R_2017, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust1R_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.94
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(10/11,2)*100, "%", sep=""), adj=1, cex = 1.5)  
      
    # cluster 2
      pred_log_clust2_2017 = predict(log_clust2, type = "response", newdata = pred2017_clust2)
      table(pred2017_clust2$team_result, pred_log_clust2_2017 >= 0.5)
      11/17 #0.65
      pred_log_clust2R_2017 = prediction(pred_log_clust2_2017, as.factor(pred2017_clust2$team_result))
      # plot ROC curve
      perf_log_clust2R_2017 = performance(pred_log_clust2R_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust2R_2017, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust2R_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.76
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(12/17,2)*100, "%", sep=""), adj=1, cex = 1.5)  
      
    # cluster 3
      pred_log_clust3_2017 = predict(log_clust3, type = "response", newdata = pred2017_clust3)
      table(pred2017_clust3$team_result, pred_log_clust3_2017 >= 0.5)
      30/43 #0.70
      pred_log_clust3R_2017 = prediction(pred_log_clust3_2017, as.factor(pred2017_clust3$team_result))
      # plot ROC curve
      perf_log_clust3R_2017 = performance(pred_log_clust3R_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust3R_2017, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust3R_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.6
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(31/43,2)*100, "%", sep=""), adj=1, cex = 1.5)  
      
    # cluster 4
      pred_log_clust4_2017 = predict(log_clust4, type = "response", newdata = pred2017_clust4)
      table(pred2017_clust4$team_result, pred_log_clust4_2017 >= 0.5)
      80/133 #0.60
      pred_log_clust4R_2017 = prediction(pred_log_clust4_2017, as.factor(pred2017_clust4$team_result))
      # plot ROC curve
      perf_log_clust4R_2017 = performance(pred_log_clust4R_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust4R_2017, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust4R_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.62
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(85/133,2)*100, "%", sep=""), adj=1, cex = 1.5)  
      
    # cluster 5
      pred_log_clust5_2017 = predict(log_clust5, type = "response", newdata = pred2017_clust5)
      table(pred2017_clust5$team_result, pred_log_clust5_2017 >= 0.5)
      87/146 #0.60
      pred_log_clust5R_2017 = prediction(pred_log_clust5_2017, as.factor(pred2017_clust5$team_result))
      # plot ROC curve
      perf_log_clust5R_2017 = performance(pred_log_clust5R_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust5R_2017, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust5R_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.57
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(90/146,2)*100, "%", sep=""), adj=1, cex = 1.5)  
      
    # cluster 6
      pred_log_clust6_2017 = predict(log_clust6, type = "response", newdata = pred2017_clust6)
      table(pred2017_clust6$team_result, pred_log_clust6_2017 >= 0.5)
      44/64 #0.69
      pred_log_clust6R_2017 = prediction(pred_log_clust6_2017, as.factor(pred2017_clust6$team_result))
      # plot ROC curve
      perf_log_clust6R_2017 = performance(pred_log_clust6R_2017, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust6R_2017, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust6R_2017, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.75
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(45/64,2)*100, "%", sep=""), adj=1, cex = 1.5)  
      