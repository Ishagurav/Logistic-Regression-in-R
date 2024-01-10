adult <- read.csv('adult_sal.csv')
library(dplyr)
adult <- select(adult,-X)
print(head(adult))
print(str(adult))
print(summary(adult))
#####################################      DATA CLEANING  #######################################
#############################    check out the frequency of the type_employer column.##########################
print(table(adult$type_employer))
##########################    combine type employer column   ##########################
unemp <- function(job){
    job <- as.character(job)
    if(job=='Never-worked' || job=='Without-pay'){
        return('Unemployed')
    }
    else {
       return(job)
    }
}
######################################    apply the function  ########################
adult$type_employer <- sapply(adult$type_employer,unemp)
print(table(adult$type_employer))
#######################################   combine State and local gov jobs into SL gov jobs  ######################
gov <- function(jtype){
    jtype <- as.character(jtype)
    if(jtype=='Local-gov' || jtype=='State-gov'){
        return('SL-gov')
    }
    else {
       return(jtype)
    }
}
adult$type_employer <- sapply(adult$type_employer,gov)
print(table(adult$type_employer))
################################   combine self employed jobs  ##########################################
emp <- function(semp) {
    semp <- as.character(semp)
    if(semp=='Self-emp-inc' || semp=='Self-emp-not-inc'){
        return('self-emp')
    }
    else {
       return(semp)
    }
}
adult$type_employer <- sapply(adult$type_employer,emp)
print(table(adult$type_employer))
#################################  Marital Column ###############################
print(table(adult$marital))
######################  reduce this to only three tables ########################
group_marr <- function(gm){
    gm <- as.character(gm)
    if(gm=='Divorced' || gm=='Separated' || gm=='Widowed'){
        return('Not-Married')
     
    }
    else if (gm=='Never-married') {
       return('Never-married')
    }
    else {
       return('Married')
    }
}
adult$marital <- sapply(adult$marital,group_marr)
print(table(adult$marital))
##############################    combine the country column   ############################
print(table(adult$country))
###################################    create vectors  #####################################
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                            'Jamaica','Trinadad&Tobago')
Other <- c('South')
#################      group them as per continent ############################################
group_country <- function(ctry){
    if (ctry %in% Asia){
        return('Asia')
    }else if (ctry %in% North.America){
        return('North.America')
    }else if (ctry %in% Europe){
        return('Europe')
    }else if (ctry %in% Latin.and.South.America){
        return('Latin.and.South.America')
    }else{
        return('Other')      
    }
}
adult$country <- sapply(adult$country,group_country)
print(table(adult$country))
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
print(str(adult))
###################    HANDEL MISSING DATA   ####################################
library(Amelia)
adult[adult == '?'] <- NA

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)
adult$income <- sapply(adult$income,factor)
print(table(adult$type_employer))
##########################         produce heatmap   ################################
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
########################   DROP MISSING DATA ###############################
adult <- na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
#######################################      EDA  #################################################
library(ggplot2)
library(dplyr)
############################  ggplot2 to create a histogram of ages, colored by income. ######################
pl <- ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
print(pl)
##############################   Plot a histogram of hours worked per week   ###############################
pl2 <- ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()
print(pl2)
################Rename the country column to region column to better reflect the factor levels###############
names(adult)[names(adult)=="country"] <- "region"
#or other method
#adult <- rename(adult,region=country)
print(str(adult))
########################## CREATE A BAR PLOT FOR REGION FOR FILL COLOR DEFINED BY REGION    #########################
pl3 <- ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(pl3)
########################## BUILDING A MODEL    #########################
print(head(adult))
########################## TRAIN TEST SPLIT    #########################

library(caTools)
########################## SET A RANDOM SEED    #########################
set.seed(101) 

########################## SPLIT THE SAMPLE    #########################
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

########################## TRAINING DATA SET    #########################
train <- subset(adult, sample == TRUE)

########################## TESTING DATA SET    #########################
test <- subset(adult, sample == FALSE)
########################## TRAINING THE MODEL   #########################
model <- glm(income ~ ., family = binomial(logit), data = train)
print(summary(model))
########################## CHOOSE A MODEL BY AIC IN A STEPWISE ALGORITHM    #########################
new.step.model <- step(model)
print(summary(new.step.model))
###################### CREATE A CONFUSION MATRIX ####################################
test$predicted.income = predict(model, newdata=test, type="response")

print(table(test$income, test$predicted.income > 0.5))
###################### CALCULATE THE ACCURACY OF YOUR MODEL ####################################
acc <- (6372+1423)/(6372+1423+548+872)
print(acc)
###################### CALCULATE THE RECALL FACTOR OF YOUR MODEL ####################################
recall <- 6732/(6372+548)
print(recall)
###################### CALCULATE THE PRECISION OF YOUR MODEL ####################################
pre <- 6732/(6372+872)
print(pre)