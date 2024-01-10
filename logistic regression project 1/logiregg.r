df.train <- read.csv("titanic_train.csv")
print(head(df.train))
print("              ")
print(str(df.train))
######################### Exploratory Data Analysis (EDA) ############################# 
library(Amelia)
library(ggplot2)
library(dplyr)
#######################   Let's explore how much missing data we have, we can use the Amelia pacakge for this  ####################
print(missmap(df.train,main='Missing Map',col=c('yellow','black'),legend=FALSE))
######################  Data Visualization with ggplot2  ########################
pl <- ggplot(df.train,aes(Survived)) + geom_bar()
pl2 <- ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))
pl3 <- ggplot(df.train, aes(Age)) + geom_histogram(bins=20,alpha=0.5,fill='blue')
print(pl3)
print(pl2)
print(pl)
#######################   Imputing the missing values by Pclass   ###############
pl4 <- ggplot(df.train,aes(Pclass,Age))
pl5 <- pl4 + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
pl6 <- pl5 + scale_y_continuous(breaks = seq(min(0),max(80),by=2)) + theme_bw()
print(pl6)
impute_age <- function(age,class){
    out <- age
    for(i in 1:length(age)){
        if(is.na(age[i])){
            if(class[i]==1){
                out[i] <- 37
            }
            else if (class[i]==2) {
               out[i] <- 29
            }
            else{
                out[i] <- 24
            }
        }
        else{
            out[i] <- age[i]
        }

    }
    return(out)
}
fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages
print(missmap(df.train,main='Imputation Check',col=c('yellow','black'),legend=FALSE))
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
print(head(df.train))
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$SibSp <- factor(df.train$SibSp)
df.train$Parch <- factor(df.train$Parch)
################################   build the model  #################################
log.model <- glm(Survived ~ .,family=binomial(link='logit'),data=df.train)
print(summary(log.model))
###############################   Predicting using Test Cases    ########################
library(caTools)
set.seed(101)
split <- sample.split(df.train$Survived,SplitRatio = 0.7)
final.train <- subset(df.train,split==TRUE)
final.test <- subset(df.train,split==FALSE)
final.log.model <- glm(Survived ~ .,family=binomial(link='logit'),data=final.train)
print(summary(final.log.model))
##################    predicting the   ###################
fitted.probabilities <- predict(final.log.model,final.test,type='response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
#######################################   ACCURACY ##################################################
misClassError <- mean(fitted.results != final.test$Survived)
print(paste('Accuracy', 1- misClassError))
###############################   Confusion matrix   ############################
print(table(final.test$Survived,fitted.probabilities > 0.5))
