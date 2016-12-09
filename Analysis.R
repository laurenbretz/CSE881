library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(data.table)
library(glmnet)
library(MASS)
options(stringsAsFactors = FALSE)


#####
# PREPROCESSING
#####

setwd("/Users/laurenbretz/Dropbox/MSU_App_Stats/Courses/Fall2016/CSE 881/Project")
rawdata <- read.csv("projdata_2000.csv")
rawdata$Attendance <- as.integer(gsub(",", "", rawdata$Attendance))  # Attendance should be numeric
rawdata$Year <- as.factor(rawdata$Year)  # Year should be caretegorical

# correct for overall record including bowl game win/loss
rawdata$Fwins <- ifelse(rawdata$Winner == rawdata$Favorite, rawdata$Fwins - 1, rawdata$Fwins)
rawdata$Flosses <- ifelse(rawdata$Winner != rawdata$Favorite, rawdata$Flosses - 1, rawdata$Flosses)
rawdata$Uwins <- ifelse(rawdata$Winner != rawdata$Favorite, rawdata$Uwins - 1, rawdata$Uwins)
rawdata$Ulosses <- ifelse(rawdata$Winner == rawdata$Favorite, rawdata$Ulosses - 1, rawdata$Ulosses)

# get win percentage in regular season
rawdata$FwinsPer <- 100*rawdata$Fwins / (rawdata$Fwins + rawdata$Flosses)
rawdata$UwinsPer <- 100*rawdata$Uwins / (rawdata$Uwins + rawdata$Ulosses)

# dependent variable: final points of favorite minus points of underdog
rawdata$diff <- ifelse(rawdata$Winner == rawdata$Favorite,
                       rawdata$Pts - rawdata$Pts.1,
                       rawdata$Pts.1 - rawdata$Pts)

# remove unneeded variables
games <- subset(rawdata, select = -c(Date, Day, Winner.Tie, Pts, Loser.Tie, Pts.1, 
                                     Winner, WinnerRank, Loser, LoserRank,
                                     favoritename, favoriteurl, underdogname, underdogurl,
                                     Fwins, Flosses, Uwins, Ulosses))


# randomize order of observations and get training & test sets
set.seed(1)
randorder <- sample(1:nrow(games))
games <- games[randorder, ]

breaknum <- round(nrow(games)*(2/3), 0)
train <- games[1:breaknum, ]
test <- games[(breaknum+1):nrow(games), ]


#####
# BASIC ANALYSIS
#####

png(file="diffhist.png")
ggplot(data = games, aes(diff)) + 
  geom_histogram(binwidth = 3) + 
  geom_vline(xintercept = 0)
dev.off()

perupset <- sum(games$diff<=0) / nrow(games)
avgspread <- mean(games$diff)

corrs <- data.table(cor( games[, c(6:ncol(games))] ))
corrs$var1 <- colnames(corrs)
highcorrs <- gather(corrs, var2, corr, F.O.PassComp:diff)
highcorrs <- highcorrs[!highcorrs$corr==1, ]
highcorrs <- highcorrs[order(-abs(highcorrs$corr)), ]
highcorrs <- highcorrs[seq(1, nrow(highcorrs), 2), ]  # keep only every other row


#####
# REGRESSION
#####

### multiple linear regression with all variables
fit_reg <- lm(diff ~ ., data = subset(train, select = -c(Bowl, Favorite, Underdog)))
rmse_fit_reg <- sqrt(mean(fit_reg$residuals^2))
summary(fit_reg)

png(file="qq_fit_reg.png")
  plot(fit_reg, which = 2)  # qq plot
dev.off()

pred_reg <- predict(fit_reg, newdata = subset(test, select = -c(Bowl, Favorite, Underdog)))
rmse_pred_reg <- sqrt(mean((pred_reg - test$diff)^2))
pred_reg_all <- predict(fit_reg, newdata = subset(games, select = -c(Bowl, Favorite, Underdog)))

### stepwise variable selection in multiple linear regression
step <- stepAIC(fit_reg, direction = "both", trace = FALSE)
fit_step <- lm(diff ~ Attendance + F.O.PassYds + F.O.Rush1st + F.D.PassComp + 
                 F.D.PassTD + F.D.RushYds + F.D.RushYdsPerAtt + F.D.RushTD + 
                 F.D.Plays + F.D.YdsPerPlay + F.D.Rush1st + F.D.Penalty1st + 
                 F.D.Penalties + F.D.PenaltyYds + Frecordrank + U.O.PassComp + 
                 U.O.PassPct + U.O.PassYds + U.O.PassTD + U.O.RushYds + U.O.YdsPerPlay + 
                 U.O.Fum + U.D.PassTD + U.D.RushYds + U.D.Plays + U.D.YdsPerPlay + 
                 U.D.Pass1st + U.D.Fum + Urecordrank + FwinsPer + UwinsPer,
               data = subset(train, select = -c(Bowl, Favorite, Underdog)))
rmse_fit_step <- sqrt(mean(fit_step$residuals^2))
summary(fit_step)

png(file="qq_fit_step.png")
  plot(fit_step, which = 2)  # qq plot
dev.off()

pred_step <- predict(fit_step, newdata = subset(test, select = -c(Bowl, Favorite, Underdog)))
rmse_pred_step <- sqrt(mean((pred_step - test$diff)^2))
pred_step_all <- predict(fit_step, newdata = subset(games, select = -c(Bowl, Favorite, Underdog)))


### ridge regression
# model.matrix tranforms categorical variables into binary dummy variables
x <- model.matrix(diff ~ ., data = subset(games, select = -c(Bowl, Favorite, Underdog)))[, -1]
y <- games$diff

# same training and test sets as before
x_train <- x[1:breaknum, ]
y_train <- y[1:breaknum]
x_test <- x[breaknum:nrow(x), ]
y_test <- y[breaknum:nrow(x)]

# find lambda by using training and test sets
set.seed(1)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)  # alpha = 0: ridge regression 
lam_ridge <- cv_ridge$lambda.min
png(file="cv_ridge.png")
  plot(cv_ridge)
dev.off()

ridge <- glmnet(x_train, y_train, alpha = 0)
fit_ridge <- predict(ridge, newx = x_train, s = lam_ridge)
fit_ridge_coef <- predict(ridge, newx = x_train, s = lam_ridge, type = "coefficients")
rmse_fit_ridge <- sqrt(mean((fit_ridge - y_train)^2))

pred_ridge <- predict(ridge, newx = x_test, s = lam_ridge)
rmse_pred_ridge <- sqrt(mean((pred_ridge - y_test)^2))

pred_ridge_all <- predict(ridge, newx = x, s = lam_ridge)


### lasso regression
# find lambda by using training and test sets
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)  # alpha = 1: lasso regression 
lam_lasso <- cv_lasso$lambda.min
png(file="cv_lasso.png")
  plot(cv_lasso)
dev.off()

lasso <- glmnet(x_train, y_train, alpha = 1)
fit_lasso <- predict(lasso, newx = x_train, s = lam_lasso)
fit_lasso_coef <- predict(lasso, newx = x_train, s = lam_lasso, type = "coefficients")
rmse_fit_lasso <- sqrt(mean((fit_lasso - y_train)^2))

pred_lasso <- predict(lasso, newx = x_test, s = lam_lasso)
rmse_pred_lasso <- sqrt(mean((pred_lasso - y_test)^2))

pred_lasso_all <- predict(lasso, newx = x, s = lam_lasso)

games2 <- cbind(games, pred_reg_all, pred_step_all, pred_ridge_all, pred_lasso_all)
colnames(games2)[85] <- "pred_ridge_all"
colnames(games2)[86] <- "pred_lasso_all"
write.csv(games2, "/Users/laurenbretz/Dropbox/MSU_App_Stats/Courses/Fall2016/CSE 881/Project/games2.csv", row.names = FALSE)