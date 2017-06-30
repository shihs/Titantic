



setting_df <- function(d) {
  d$Title <- NA
  d$Title[grep("Mr", d$Name)] <- "Mr"
  d$Title[grep("Dr", d$Name)] <- "Dr"
  d$Title[grep("Mrs", d$Name)] <- "Mrs"
  d$Title[grep("Rev", d$Name)] <- "Rev"
  d$Title[grep("Col", d$Name)] <- "Col"
  d$Title[grep("Miss", d$Name)] <- "Miss"
  d$Title[grep("Master", d$Name)] <- "Master"
  d[which(is.na(d$Title)), "Title"] <- "no title"
  
  d$Pclass <- as.character(d$Pclass)
  
  d$relative <- d$SibSp + d$Parch
  # d[which(is.na(d$Age)), "Age"] <- mean(d$Age, na.rm = TRUE)
  
  
  pos <- which(is.na(d$Age))
  
  for (title in unique(d$Title)) {
    i <- which(d$Title == title)
    d[intersect(pos, i), "Age"] <- median(d[setdiff(i, pos), "Age"])
  }
  
# 
#   barplot(table(d$relative, round(d$Age, 0)), col = rainbow(10), legend = TRUE)
#   barplot(table(d$Parch, round(d$Age, 0)), col = rainbow(10), legend = TRUE)
#   barplot(table(d$SibSp, round(d$Age, 0)), col = rainbow(10), legend = TRUE)
#   barplot(table(d$title, round(d$Age, 0)), col = rainbow(10), legend = TRUE)
  
  # i <- intersect(which(d[, "Parch"] == 0), which(d[, "SibSp"] != 0))
  # barplot(table(d[i, "Age"]))
  # barplot(table(d[i, "title"], d[i, "Age"]), col= (rainbow(10)), legend = TRUE)
  # barplot(table(d[i, "Pclass"], d[i, "Age"]), col= (rainbow(10)), legend = TRUE)
  
  # i <- which(is.na(d$Age))
  # d[i, "Age"] <- ifelse((d[i, "relative"] != 0 & d[i, "Title"] == "no title"), 6, d[i, "Age"])
  # i <- which(is.na(d$Age))
  # d[i, "Age"] <- ifelse((d[i, "relative"] == 0 & d[i, "Title"] == "Miss"), 25, d[i, "Age"])
  # i <- which(is.na(d$Age))
  # d[i, "Age"] <- ifelse((d[i, "relative"] == 0 & d[i, "Pclass"] == "3"), 30, d[i, "Age"])
  # i <- which(is.na(d$Age))
  # d[i, "Age"] <- ifelse((d[i, "Parch"] == 0 & d[i, "SibSp"] != 0), 35, d[i, "Age"])
  # i <- which(is.na(d$Age))
  # d[i, "Age"] <- ifelse(d[i, "relative"] >= 7, 9, d[i, "Age"])
  # i <- which(is.na(d$Age))
  # d[i, "Age"] <- ifelse((d[i, "relative"] == 0 & d[i, "Pclass"] == "1"), 60, d[i, "Age"])
  # i <- which(is.na(d$Age))
  # d[i, "Age"] <- ifelse((d[i, "relative"] == 0 & d[i, "Pclass"] == "2"), 45, d[i, "Age"])
  # d[i, "Age"] <- 20
  # 
  d$Age_class <- ifelse(d$Age <= 12, "child", "adult")
  
  d$Cabin_class <- substr(d$Cabin, 1, 1)
  d$Cabin_class <- ifelse(d$Cabin == "", "Unknown", d$Cabin_class)
  
  d$interact_class <- d$Age*as.integer(d$Pclass)
  d$interact_sibsp <- d$Age*d$SibSp
  
  # barplot(table(d$Survived, round(d$Fare, 0)))
  # barplot(table(d$Pclass, round(d$Fare, 0)))
  # d$Fare_class <- ifelse(d$Fare >= 75, "1", ifelse(d$Fare > 25, "2", "3"))
  
  d$Fare_per_person <- round(d$Fare/(d$relative+1), 1)
  
  d$Family_type <- ifelse(d$relative + 1 == 1, "Sigleton", 
                          ifelse(d$relative + 1 > 5, "LargeFamily", "SmallFamily"))
  
  return(d)
  
}


raw <- read.csv("train.csv", stringsAsFactors = FALSE)
raw <- setting_df(raw)


# set.seed(10)
# train.index <- sample(x=1:nrow(raw), size=ceiling(0.8*nrow(raw) ))
# train <- raw[train.index, ]
# test <- raw[-train.index, ]


train <- raw

#======glm======
# glm.col <- c("Survived", "Pclass", "Sex", "SibSp", "Age", "Parch", "Title", "Age_class", "cabin_class", "Fare_class")
glm.col <-  c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Age_class", "Cabin_class", "Fare_per_person", 
              "Embarked", "Title", "Fare", "Age", "interact_class", "interact_sibsp", "Family_type")

t <- train[, glm.col]
glm.model <- glm(Survived ~ . , data = t)


t <- cbind(pred = predict.glm(glm.model, type = "response", t), t)
t$result <- ifelse(t$pred>0.6, 1, 0)
# 
# 
step.model <- step(glm.model)
t <- train[, glm.col]
t <- cbind(pred = predict.glm(step.model, type = "response", t), t)
t$result <- ifelse(t$pred>0.6, 1, 0)

library(ROCR)
# plot ROC
roc_pred <- prediction(t$result, t$Survived)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
abline(0, 1, col = "grey")
# 
# get area under the curve
performance(roc_pred,"auc")@y.values
# 
# 
table(real=t$Survived, predict=t$result)



test <- read.csv("test.csv", stringsAsFactors = FALSE)
test <- setting_df(test)

test[153, "Fare"] <- mean(test[which(test$Pclass==3), "Fare_per_person"], na.rm = TRUE)
test[153, "Fare_per_person"] <- mean(test[which(test$Pclass==3), "Fare_per_person"], na.rm = TRUE)

library(caret)
test <- cbind(pred = predict(glm.model, newdata = test, type = "response"), test)
test$Survived <- ifelse(test$pred > 0.6, 1, 0)
test <- test[, c("PassengerId", "Survived")]


write.csv(test, "test_result.csv", row.names = FALSE)



#=====decision tree=======
# exclude Mr.
tree.col <- c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Age_class", "Cabin_class", "Fare_per_person", 
              "Embarked", "relative", "Title", "Fare", "Age", "interact_class", "interact_sibsp", "Family_type")

t <- train[-which(train$Title %in% c("Mr", "Dr", "Col", "no title", "Rev")), tree.col]
# t <- train[-which(train$Fare_per_person >= 26), tree.col]
# t <- train[, tree.col]
t$Survived <- as.character(t$Survived)

# t$Age <- NULL

tree.other.model <- rpart(Survived ~ ., data = t)

t <- cbind(result = predict(tree.other.model, newdata = t, type = "class"), t)
table(real=t$Survived, predict=t$result)



# require(rpart.plot)
prp(tree.other.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    extra=2)




# Mr.
# tree.col <- c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Age_class", "Fare", "Embarked", "relative", "Title", "Age", "Fare_per_person")
# tree.col <- c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Age_class", "Cabin_class", "Fare_per_person", 
#               "Embarked", "relative", "Title", "Fare", "Age", "interact_class", "interact_sibsp")
t <- train[which(train$Title %in% c("Mr", "Dr", "Col", "no title", "Rev")), tree.col]
# t <- train[-which(train$Fare_per_person >= 26), tree.col]
t$Survived <- as.character(t$Survived)


tree.Mr.model <- rpart(Survived ~ ., data = t)

t <- cbind(result = predict(tree.Mr.model, newdata = t, type = "class"), t)
table(real=t$Survived, predict=t$result)



# require(rpart.plot)
prp(tree.Mr.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    extra=2)




test <- read.csv("test.csv", stringsAsFactors = FALSE)
test <- setting_df(test)

# test <- cbind(Survived = predict(tree.other.model, newdata=test, type="class"), test)


test.other <- test[-which(test$Title %in% c("Mr", "Dr", "Col", "no title", "Rev")), ]
test.other <- cbind(Survived = predict(tree.other.model, newdata=test.other, type="class"), test.other)


test.Mr <- test[which(test$Title %in% c("Mr", "Dr", "Col", "no title", "Rev")), ]
test.Mr <- cbind(Survived = predict(tree.Mr.model, newdata=test.Mr, type="class"), test.Mr)

test <- rbind(test.Mr, test.other)

# Survived <- predict(tree.model, newdata=test, type="class")
# 
# test <- cbind(Survived, test)
test <- test[, c("PassengerId", "Survived")]

write.csv(test, "test_result.csv", row.names = FALSE)








#================================

library(ROCR)
# plot ROC
roc_pred <- prediction(result$result, result$Survived)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
abline(0, 1, col = "grey")

# get area under the curve
performance(roc_pred,"auc")@y.values

#cutoff point
# acc.perf = performance(roc_pred, measure = "acc")
# plot(acc.perf)




