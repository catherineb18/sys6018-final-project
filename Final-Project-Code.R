# Catherine Beazley
# SYS 6018: Data Mining
# Final Project

library(stringr)
library(leaps)
library(tree)
library(randomForest)

##############################################################################
#                                                                            #
#                     Data Merging and Cleaning                              #
#                                                                            #
##############################################################################

setwd('C:/Users/cathe/Desktop/SYS6018/Final_Project')

# Merging Code from UC Irvine Data Description. There are two datasets-- one with
# for Mathematics grades and one for Portuguese Language grades. These two datasets
# can be merged with the merging code provided by UC Irvine below, and the result is 
# a dataset describing students who are included in both the math dataset and the
# language dataset. The intersection accounts for 392 students.

math <- read.table("student-mat.txt",sep=";",header=TRUE)
language <- read.table("student-por.txt",sep=";",header=TRUE)
both <- merge(math,language,by=c("school","sex","age","address","famsize","Pstatus",
                                 "Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

print(nrow(both)) # 382 students

# There are overlapping columns in the dataset that aren't used in the merge but contain the 
# same data. In some of these, there are a couple differences between the two. The difference is
# because the students filled out the seperate but the same form twice. I am deleting the second
# copy of columns and renaming the columns so that there is not '.x' at the end, so in the rare
# case that the columns contained different entries, I am keeping the first entry in the data.

# "guardian.x"   "traveltime.x" "studytime.x"  "failures.x"   "schoolsup.x" 
# "famsup.x"     "paid.x"       "activities.x" "higher.x"     "romantic.x"   "famrel.x"    
# "freetime.x"   "goout.x"      "Dalc.x"       "Walc.x"       "health.x"     "absences.x" 

# "guardian.y"   "traveltime.y" "studytime.y" 
# "failures.y"   "schoolsup.y"  "famsup.y"     "paid.y"       "activities.y" "higher.y"    
# "romantic.y"   "famrel.y"     "freetime.y"   "goout.y"      "Dalc.y"       "Walc.y"      
# "health.y"     "absences.y" 

# The math versions of the columns are column indices 14-30 and the language versions of the
# repeat columns are 34-50

# Removing the repeated columns which are the ones with .y
both <- both[,c(1:33, 51:length(colnames(both)))]

# Removing the '.x'
colnames(both)[14:30] <- str_replace(colnames(both)[14:30], '.x', '')

# Renaming the grade columns G1.x through G3.y
colnames(both)[31:36] <- c('MathGrade1', 'MathGrade2', 'MathFinal', 'LanguageGrade1', 'LanguageGrade2', 'LanguageFinal')

# Checking that the columns have correct data type
types <- c()
for(i in 1:36){
  types <- c(types, class(both[,i]))
}
names(types) <- colnames(both)
types

# Writing it to a csv for the final submission
write.csv(both, 'student_performance_data_merged.csv')

# Since I am predicting Math final grade and language final grade separately, I am subsetting the combined
# dataset into language and math, where both contain the same social and economic descriptors in columns
# 1 to 30, and then the corresponding three grades.I could keep them together, but separating them makes
# things easier to conceptualize and to compute.
lang <- both[,c(1:30, 34:36)]
math <- both[, 1:33]

# Things to note about the data:
# - The grades are on a 20 points grading system, where a 20 is a perfect score

##############################################################################
#                                                                            #
#                           Data Exploration                                 #
#                                                                            #
##############################################################################



################# Summary Stats of Response ################################
summary(lang$LanguageFinal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   11.00   13.00   12.52   14.00   19.00 

summary(math$MathFinal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    8.00   11.00   10.39   14.00   20.00 

# Math grades on average seem to be lower

######### Looking at Distributions in Histograms and Bar Charts of Variables #########

for(i in 1:length(colnames(both))){
  if(class(both[,i]) != 'factor'){
    hist(both[,i], xlab=colnames(both)[i], main = colnames(both)[i])
  } else{
    barplot(table(both[,i]), main=colnames(both)[i], xlab=colnames(both)[i]) 
  }
}
# Numerical Variables:
#   -Variables that are skewed right: absences, Walc, Dalc, failures, studytime, traveltime, age, 
#   -Variables that are skewed left: famrel
#   -Variables that are uniform: Fedu
# Rest are normally distributed

# Categorical Variables:
#   -School: Many more students in GP than MS
#   -Sex: Pretty evenly split
#   -Address: Many more 'U' than 'R'
#   -Famsize: Many more GT3 than LE3
#   -Pstatus: Many more 'T' than 'A'
#   -Mjob: 
#   -Fjob: Mostly other
#   -Reason: somewhat well split except other is much less
#   -nursery: mostly yes
#   -internet: mostly yes
#   -guardian: mostly mother
#   -schoolsup: mostly no
#   -famsup: more yes than no but not that much more
#   -paid: pretty evenly split
#   -activities: mostly yes
#   -higher: mostly yes
#   -romantic: mostly no but yes is still well represented

# Interesting features-- the language grades follow a normal distribution more closely while
# the math grades seem to be more bimodal, with another mode at a low score. The math grades
# also have a mean that appears lower than the mean for the language grades.

# The skews in the numerical features all make sense, since one would expect predominantly lower
# means for number of absences, number of students who drink on weekdays and weekends, failures, 
# studytime, traveltime, and age since the population are school kids. 

# Some of the categorical variables have an overwhelming majority, like higher, schoolsup, internet,
# Pstatus, and school

############## Plotting Numerical Regressors vs Response ########################
for(i in 1:length(colnames(lang))){
  if(class(lang[,i]) != 'factor'){
    plot(x=lang[,i], y= lang[,'LanguageFinal'], xlab=colnames(lang)[i], ylab='LanguageFinal', main = colnames(lang)[i])
  } 
}
# Negative linear slope with failures, traveltime, and Dalc.
# Positive Slope with LangaugeGrade1 and LanguageGrade2.
# Rest do not show a clear partial relationship with Final Grade.

for(i in 1:length(colnames(math))){
  if(class(math[,i]) != 'factor'){
    plot(x=math[,i], y= math[,'MathFinal'], xlab=colnames(math)[i], ylab='MathFinal', main = colnames(math)[i])
  } 
}
# Positive linear relationship between MathFinal and MathGrade1, MathGrade2, 
# Negative linear relationship with Walc, Dalc, failures, traveltime
# Rest do not show a clear partial relationship with Final Grade.

##############################################################################
#                                                                            #
#               Splitting Data into Training and Validation                  #
#                                                                            #
##############################################################################


# Training on 80% of the data and testing on 20%. Will build all models on  training set.
set.seed(1996)
train <- sample(1:382, 382/2) # both sets have 382 rows
lang.train <- lang[train,]
lang.valid <- lang[-train,]
lang.train.nogrades <- lang[train,-(31:32)]
lang.valid.nogrades <- lang[-train,-(31:32)]
math.train <- math[train,]
math.valid <- math[-train,]
math.train.nogrades <- math[train,-(31:32)]
math.valid.nogrades <- math[-train,-(31:32)]

##############################################################################
#                                                                            #
#                     OLS with Subset Selection                              #
#                                                                            #
##############################################################################

# Rationale for this model: Both response variables look normally distributed. As noted above, some
# of the regressor variables seem to have a relationship with the response. Fitting a linear model, then
# testing for model adequacy could show if a linear or non linear model can explain the data.

# This function makes a table of all regressors with categorical variables encoded, and 4 statistics
# used for comparing all possible models. When a 1 is under a regressors, that regressor was included 
# in the model.
best.selection <- function(model.name, data, indices.regressors){
  R2.seq <- summary(model.name)$rsq
  adj.R2.seq <- summary(model.name)$adjr2
  Cp.seq <- summary(model.name)$cp
  BIC.seq <- summary(model.name)$bic
  
  SS.Res.seq <- summary(model.name)$rss
  n <- dim(data)[1]
  k.seq <- as.numeric(row.names(summary(model.name)$which))
  p.seq <- k.seq + 1
  MS.Res.seq <- SS.Res.seq / (n - p.seq)
  AIC.seq <- n*log(SS.Res.seq / n) + 2*p.seq
  
  disp.submod <- cbind(summary(model.name)$which[,indices.regressors], adj.R2.seq, Cp.seq, AIC.seq, BIC.seq)
  return(disp.submod)
}

############ Subset Selection for Language (with prev. grades) ###############

all.poss.lang <- regsubsets(LanguageFinal ~., data = lang.train, method = "seqrep", nbest=5)
summary(all.poss.lang) # Used this to get the indices of the regressors including encoding of categorical ones
disp.submod <- best.selection(all.poss.lang, lang.train,1:42)
disp.submod <- as.data.frame(disp.submod)

# Sorting by Adjusted R2 because want to maximize this
disp.submod[order(disp.submod$adj.R2.seq, decreasing = T),c(1:42, 43)]
# Top 5 models variables subsets:
# 1. famsizeLE3, PstatusT, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2  
# 2. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade1, LanguageGrade2
# 3. famsizeLE3, Fjobteacher, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2
# 4. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, goout, LanguageGrade2
# 5. famsizeLE3, Fjobservices, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2

# Sorting by Cp because want to minimize this
disp.submod[order(disp.submod$Cp.seq),c(1:42, 44)]
# Top 5 models variables subsets:
# 1. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2
# 2. famsizeLE3, PstatusT, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2
# 3. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade1, LanguageGrade2
# 4. famsizeLE3, Fjobteacher, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2
# 5. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, goout, LanguageGrade2

# Sorting by AIC because want to minimize this
disp.submod[order(disp.submod$AIC.seq),c(1:42, 45)]
# Top 5 models variables subsets:
# 1. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2
# 2. famsizeLE3, PstatusT, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2
# 3. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade1, LanguageGrade2
# 4. famsizeLE3, Fjobteacher, reasonother, nurseryyes, failures, schoolsupyes, higheryes, LanguageGrade2
# 5. famsizeLE3, reasonother, nurseryyes, failures, schoolsupyes, higheryes, goout, LanguageGrade2

# Sorting by BIC because want to minimize this
disp.submod[order(disp.submod$BIC.seq),c(1:42, 46)]
# Top 5 models variables subsets:
# 1. nurseryyes, higheryes, LanguageGrade2
# 2. higheryes, LanguageGrade2
# 3. reasonother, higheryes, LanguageGrade2
# 4. famsizeLE3, nurseryyes, higheryes, LanguageGrade2
# 5. famsizeLE3, reasonother, nurseryyes, higheryes, LanguageGrade2

# Overall, all the diagnostic seem to suggest a similar ranking of models except for BIC. Because of the
# consistency among the statistics adjusted R^2, Mallow's Cp, and AIC, I am taking the best models to 
# consist of famsize, reason, nursery, failures, schoolsup, higher, and LanguageGrade2.

lm.lang.withgrades <- lm(LanguageFinal ~ famsize+reason+nursery+failures+schoolsup+higher+LanguageGrade2,
                         data = lang.train)

############ Subset Selection for Language (without prev. grades) ###############
all.poss.lang.nogrades <- regsubsets(LanguageFinal ~., data = lang.train.nogrades, method = "seqrep", nbest=5)
summary(all.poss.lang.nogrades) # Used this to get the indices of the regressors including encoding of categorical ones
disp.submod.nogrades <- best.selection(all.poss.lang.nogrades, lang.train.nogrades, 1:40)

# Sorting by Adjusted R2 because want to maximize this
disp.submod.nogrades[order(-disp.submod.nogrades[,'adj.R2.seq']), 1:41]
# Top 5 models variables subsets:
# 1. Fjobother, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 2. Mjobother, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 3. schoolMS, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 4. Medu, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 5. PstatusT, studytime, failures, schoolsupyes, higheryes, Dalc, health

# Sorting by Cp because want to minimize this
disp.submod.nogrades[order(disp.submod.nogrades[,'Cp.seq']), c(1:40, 42)]
# Top 5 models variables subsets:
# 1. Fjobother, studytime failures schoolsupyes, higheryes, Dalc, health
# 2. Mjobother, studytime failures schoolsupyes, higheryes, Dalc, health
# 3. schoolMS, studytime failures schoolsupyes, higheryes, Dalc, health
# 4. Medu, studytime failures schoolsupyes, higheryes, Dalc, health
# 5. PstatusT, studytime failures schoolsupyes, higheryes, Dalc, health

# Sorting by AIC because want to minimize this
disp.submod.nogrades[order(disp.submod.nogrades[,'AIC.seq']), c(1:40, 43)]
# Top 5 models variables subsets:
# 1. Fjobother, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 2. Mjobother, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 3. schoolMS, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 4. Medu, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 5. PstatusT, studytime, failures, schoolsupyes, higheryes, Dalc, health

# Sorting by BIC because want to minimize this
disp.submod.nogrades[order(disp.submod.nogrades[,'BIC.seq']), c(1:40, 44)]
# Top 5 models variables subsets:
# 1. studytime, failures, schoolsupyes, higheryes, Dalc, health
# 2. Fjobother, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 3. Mjobother, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 4. schoolMS, studytime, failures, schoolsupyes, higheryes, Dalc, health
# 5. Medu, studytime, failures, schoolsupyes, higheryes, Dalc, health

# Among all the diagnostics for determining the best subset of regressors for the model,
# there seems to be consistency that the subset Fjob, studytime, failures, schoolsup, 
# higher, Dalc, health make up the best linear model

lm.lang.withoutgrades <- lm(LanguageFinal~Fjob+studytime+failures+schoolsup+higher+Dalc+health, 
                            data=lang.train.nogrades)


############ Subset Selection for Math (with prev. grades) ###############
all.poss.math <- regsubsets(MathFinal ~., data = math.train, method = "seqrep", nbest=5)
all.poss.math # Used this to get the indices of the regressors including encoding of categorical ones
disp.submod <- best.selection(all.poss.math, math.train, 1:42)

# Sorting by Adjusted R2 because want to maximize this
disp.submod[order(-disp.submod[,'adj.R2.seq']), c(1:42,43)]
# Top 5 models variables subsets:
# 1. failures, romanticyes, famrel, freetime, goout, absences, MathGrade1, MathGrade2 
# 2. failures, famrel, freetime, goout, Walc, absences, MathGrade1, MathGrade2 
# 3. failures, activitiesyes, famrel, freetime, goout, absences, MathGrade1, MathGrade2 
# 4. Fedu, failures, famrel, freetime, goout, absences, MathGrade1, MathGrade2 
# 5. failures, famrel, freetime, goout, absences, MathGrade1, MathGrade2 

# Sorting by Cp because want to minimize this
disp.submod[order(disp.submod.nogrades[,'Cp.seq']), c(1:42,44)]
# Top 5 models variables subsets:
# 1. sexM, failures, famrel, absences, MathGrade1, MathGrade2
# 2. Fedu, failures, famrel, absences, MathGrade1, MathGrade2
# 3. failures, romanticyes, famrel, absences, MathGrade1, MathGrade2
# 4. guardianother, failures, famrel, absences, MathGrade1, MathGrade2
# 5. failures, famrel, freetime, goout, absences, MathGrade1, MathGrade2

# Sorting by AIC because want to minimize this
disp.submod[order(disp.submod.nogrades[,'AIC.seq']), c(1:42,45)]
# Top 5 models variables subsets:
# 1. sexM, failures, famrel, absences, MathGrade1, MathGrade2
# 2. Fedu, failures, famrel, absences, MathGrade1, MathGrade2
# 3. failures, romanticyes, famrel, absences, MathGrade1, MathGrade2
# 4. guardianother, failures, famrel, absences, MathGrade1, MathGrade2
# 5. failures, famrel, freetime, goout, absences, MathGrade1, MathGrade2

# Sorting by BIC because want to minimize this
disp.submod[order(disp.submod.nogrades[,'BIC.seq']), c(1:42,46)]
# Top 5 models variables subsets:
# 1. failures, famrel, freetime, MathGrade1, MathGrade2
# 2. sexM, failures, famrel, absences, MathGrade1, MathGrade2
# 3. Fedu, failures, famrel, absences, MathGrade1, MathGrade2
# 4. failures, romanticyes, famrel, absences, MathGrade1, MathGrade2
# 5. guardianother, failures, famrel, absences, MathGrade1, MathGrade2

# Among all the diagnostics for determining the best subset of regressors for the model,
# there seems to be consistency that the subset sex, failures, famrel, absences, MathGrade1, 
# and MathGrade2 make up the best model.

lm.math.withgrades <- lm(MathFinal ~ sex + failures + famrel + absences + MathGrade1 + MathGrade2, 
                         data = math.train)



############ Subset Selection for Math (without prev. grades) ###############
all.poss.math.nogrades <- regsubsets(MathFinal ~., data = math.train.nogrades, method = "seqrep", nbest=5)
all.poss.math.nogrades # Used this to get number of regressors including encoding of categorical ones
disp.submod.m.nogrades <- best.selection(all.poss.math.nogrades, math.train.nogrades, 1:40)

# Sorting by Adjusted R2 because want to maximize this
disp.submod.m.nogrades[order(-disp.submod.m.nogrades[,'adj.R2.seq']), c(1:40,41)]
# Top 5 models variables subsets:
# 1. sexM, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, romanticyes, goout 
# 2. sexM, famsizeLE3, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout 
# 3. sexM, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout, health
# 4. sexM, Mjobservices, Fjobteacher, reasonreputation, internetyes, traveltime, failures, goout 
# 5. sexM, Mjobhealth, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout

# Sorting by Cp because want to minimize this
disp.submod.m.nogrades[order(disp.submod.m.nogrades[,'Cp.seq']), c(1:40,42)]
# Top 5 models variables subsets:
# 1. sexM, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, romanticyes, goout 
# 2. sexM, famsizeLE3, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout 
# 3. sexM, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout, health
# 4. sexM, Mjobservices, Fjobteacher, reasonreputation, internetyes, traveltime, failures, goout 
# 5. sexM, Mjobhealth, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout

# Sorting by AIC because want to minimize this
disp.submod.m.nogrades[order(disp.submod.m.nogrades[,'AIC.seq']), c(1:40,43)]
# Top 5 models variables subsets:
# 1. sexM, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, romanticyes, goout
# 2. sexM, famsizeLE3, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout
# 3. sexM, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout, health
# 4. sexM, Mjobservices, Fjobteacher, reasonreputation, internetyes, traveltime, failures, goout
# 5. sexM, Mjobservices, Fjobteacher, reasonreputation, traveltime, failures, goout

# Sorting by BIC because want to minimize this
disp.submod.m.nogrades[order(disp.submod.m.nogrades[,'BIC.seq']), c(1:40,44)]
# Top 5 models variables subsets:
# 1. sexM, Mjobservices, reasonreputation, traveltime, failures, romanticyes, goout
# 2. sexM, Mjobservices, traveltime, failures, goout
# 3. sexM, Mjobservices, reasonreputation, failures, goout
# 4. sexM, Mjobservices, failures, goout
# 5. sexM, Mjobservices, traveltime, failures, goout, health

# Among all the diagnostics for determining the best subset of regressors for the model,
# there seems to be consistency that the subset sex, Mjob, Fjob, reason, traveltime, failures,
# romantic, and goout 

lm.math.withoutgrades <- lm(MathFinal ~ sex + Mjob + Fjob + reason + traveltime + failures + romantic + 
                              goout, data = math.train.nogrades) 

##############################################################################
#                                                                            #
#                           Decision Tree                                    #
#                                                                            #
##############################################################################

####################### Language -- With Grades ##############################
tree.lg <- tree(LanguageFinal ~ ., lang.train)
summary(tree.lg)
# Variables actually used in tree construction:
# [1] "LanguageGrade2" "reason"   

####################### Language -- No Grades ################################
tree.lng <- tree(LanguageFinal ~ ., lang.train.nogrades)
summary(tree.lng)
# Variables actually used in tree construction:
# [1] "failures"   "traveltime" "schoolsup"  "Fjob"       "studytime"  "Medu"       "paid"      
# [8] "romantic"   "health"     "famsup"     "reason"     "Mjob"       "famsize"    "Fedu"      
# [15] "higher" 

######################### Math -- With Grades ################################
tree.mg <- tree(MathFinal ~ ., math.train)
summary(tree.mg)
# Variables actually used in tree construction:
# [1] "MathGrade2" "absences"   "reason"

######################### Math -- No Grades ##################################
tree.mng <- tree(MathFinal ~ ., math.train.nogrades)
summary(tree.mng)
# Variables actually used in tree construction:
# [1] "failures"   "Mjob"       "traveltime" "sex"        "guardian"   "goout"      "age"       
# [8] "famsup"     "absences"   "studytime"  "health"     "schoolsup"  "romantic"   "Fjob"   

##############################################################################
#                                                                            #
#                                Bagging                                     #
#                                                                            #
##############################################################################


####################### Language -- With Grades ##############################
m <- dim(lang.train)[2] - 1 # m = number of predictors, so bagging
bag.lang.g <- randomForest(LanguageFinal ~ ., data = lang.train, mtry = m, importance=T)
varImpPlot(bag.lang.g)
importance(bag.lang.g)
#                    %IncMSE IncNodePurity
# school         -1.42109809     0.4989374
# sex            -0.75250540     1.6924629
# age             1.09406680     6.8138636
# address        -0.52446663     5.8622215
# famsize        -1.00901418     5.6054880
# Pstatus        -1.58359135     0.8919277
# Medu            0.81913919     8.7315759
# Fedu            2.17919802     7.6257230
# Mjob           -0.78652326     7.8005416
# Fjob            3.77847234    16.5465620
# reason         -1.08396829    12.5727198
# nursery         0.28637492     2.2352676
# internet       -0.55707784     0.5197204
# guardian        2.62226828     4.3978234
# traveltime     -2.53651885     2.3017571
# studytime      -0.37476926     3.5263981
# failures       -4.17072592    26.2074778
# schoolsup      -2.77955408     1.4445981
# famsup         -1.92306731     1.7578032
# paid            3.18267974     3.0622417
# activities      0.74822969     1.6493473
# higher         -3.71054444    10.9467522
# romantic       -2.72018556     2.5423505
# famrel         -0.09824734    42.5179674
# freetime        1.39941535     5.8107997
# goout           1.26061361     8.6997434
# Dalc           -1.95258540     6.3521291
# Walc           -2.76637661    14.5673866
# health         -3.80562176     5.5469095
# absences       -0.26730553     9.3526294
# LanguageGrade1 14.02316098    46.6668538
# LanguageGrade2 78.20350803  1290.5663717

# LanguageGrade1 and LanguageGrade2 are by far the most important predictors, distantly
# followed by famrel, failures, and Fjob.

####################### Language -- Without Grades ##############################
m <- dim(lang.train.nogrades)[2] - 1 # m = number of predictors, so bagging
bag.lang.ng <- randomForest(LanguageFinal ~ ., data = lang.train.nogrades, mtry = m, importance=T)
varImpPlot(bag.lang.ng)
importance(bag.lang.ng)
#                %IncMSE IncNodePurity
# school      1.49456938     11.826861
# sex         1.56796830     21.626780
# age         3.14301822     39.229040
# address     1.39493313     14.242395
# famsize     1.30757116     19.850636
# Pstatus    -0.41478146     12.341752
# Medu        4.14390259     52.695527
# Fedu        6.80568066     80.114029
# Mjob        0.38829968     67.201764
# Fjob        3.66674133     79.546986
# reason     -1.59674688     60.730142
# nursery    -0.07264847     11.647038
# internet   -0.87580122      6.423772
# guardian   -1.02318638     22.224212
# traveltime  7.76625871     73.161264
# studytime   5.81125327     65.873621
# failures   22.96347394    277.647040
# schoolsup  14.53842876     56.306918
# famsup      2.04475640     12.792390
# paid        2.45480099     15.293480
# activities  2.34099442     16.772339
# higher      7.38373135     98.620548
# romantic   -0.74998156     15.273230
# famrel      2.36956632     67.714207
# freetime   -1.28962542     47.229102
# goout       2.23120517     49.255901
# Dalc       -0.39516742     51.348034
# Walc        2.63993819     65.425426
# health      3.85962966     63.624615
# absences   -1.16622348     60.265885

# failures, higher, Fedu, and studytime seem to be the most important variables

####################### Math -- With Grades ##############################
m <- dim(math.train)[2] - 1 # m = number of predictors, so bagging
bag.math.g <- randomForest(MathFinal ~ ., data = math.train, mtry = m, importance=T)
varImpPlot(bag.math.g)
importance(bag.math.g)
#                %IncMSE IncNodePurity
# school     -1.17593306  5.858315e+00
# sex         1.44776698  1.751527e+00
# age        -0.07969658  2.182480e+01
# address     0.08867897  8.204340e-01
# famsize    -1.04394439  6.803260e+00
# Pstatus    -1.16719837  1.843214e+00
# Medu        0.07541849  6.804559e+00
# Fedu       -0.32068482  4.883953e+00
# Mjob        3.44490524  3.735304e+01
# Fjob        1.00383475  1.948352e+01
# reason      1.54140799  2.841854e+01
# nursery     2.40820922  4.686045e+00
# internet    0.47065337  2.093101e+00
# guardian    2.08924281  3.446641e+00
# traveltime -0.84539144  4.051678e+00
# studytime  -0.48281391  1.191037e+01
# failures    2.03390322  1.186339e+01
# schoolsup   1.77436058  7.178333e+00
# famsup      0.75587962  3.086957e+00
# paid        4.94154055  4.407829e+00
# activities -1.12767757  4.393199e+00
# higher     -1.41695845  5.316587e-02
# romantic   -1.01660971  3.340480e+00
# famrel      0.87497479  7.003957e+00
# freetime    1.16934699  4.276816e+00
# goout       0.53131831  6.862489e+00
# Dalc       -0.88465022  8.800559e+00
# Walc        2.19325050  9.692987e+00
# health      0.13226264  1.411269e+01
# absences   30.59677477  4.114609e+02
# MathGrade1  7.54813082  2.815733e+01
# MathGrade2 95.97408836  3.166748e+03

# MathGrade2, absences, MathGrade1, and Mjob seem to be the most important

####################### Math -- Without Grades ##############################
m <- dim(math.train.nogrades)[2] - 1 # m = number of predictors, so bagging
bag.math.ng <- randomForest(MathFinal ~ ., data = math.train.nogrades, mtry = m, importance=T)
varImpPlot(bag.math.ng)
importance(bag.math.ng)
#                %IncMSE IncNodePurity
# school      4.24064327      22.40607
# sex        10.51623047     135.12206
# age         1.69684169     106.83495
# address     0.11631100      42.19210
# famsize     1.76442909      54.49072
# Pstatus     1.48284211      22.32235
# Medu        1.74311439      92.76380
# Fedu        1.18578471      84.02606
# Mjob        2.76888165     259.56028
# Fjob        3.43757669     190.41838
# reason      2.98062604     171.47328
# nursery     1.28359555      18.52714
# internet    2.24230263      41.03173
# guardian    5.69964484      86.25180
# traveltime  4.30080167     169.33606
# studytime   2.79682959     104.41213
# failures   22.49783638     660.24509
# schoolsup   4.80527260      63.28618
# famsup     -1.24616570      42.02880
# paid        2.07316064      38.39474
# activities  0.67627481      30.88915
# higher      4.16722812      35.66670
# romantic    1.81093844      51.35218
# famrel     -1.89545371      66.16152
# freetime   -1.96171301      74.44674
# goout       4.75356008     213.68527
# Dalc       -0.41766401      24.24023
# Walc        0.03628649     113.35679
# health      1.74495825     111.58662
# absences   17.39949549     576.34947

# Failures, absences, Mjob, goout seem to be the most important variables.

##############################################################################
#                                                                            #
#                           Random Forest                                    #
#                                                                            #
##############################################################################

####################### Language -- With Grades ##############################
m <- sqrt(dim(lang.train)[2] -1) # m = sqrt(p)
rf.lang.g <- randomForest(LanguageFinal ~ ., data = lang.train, mtry = m, importance=T)
varImpPlot(rf.lang.g)
importance(rf.lang.g)
#                    %IncMSE IncNodePurity
# school         -0.97758370      9.760197
# sex             0.85871367      7.087928
# age             1.07654693     20.994403
# address         1.47217467      9.647670
# famsize         2.09765574      8.286712
# Pstatus        -0.32673377      5.050037
# Medu            1.37040314     24.203234
# Fedu            3.90688680     33.215769
# Mjob           -0.45254724     24.765200
# Fjob            1.36304853     29.970031
# reason          0.09779597     26.954760
# nursery         1.26682537      8.406374
# internet       -2.19374346      4.454090
# guardian        0.31517684      9.100470
# traveltime      4.02525684     21.515792
# studytime       0.89646395     30.522473
# failures        6.59358457     87.666010
# schoolsup       4.22259516     11.126378
# famsup          0.84483419      7.368466
# paid            2.49699652      9.425074
# activities      1.14850925      7.745398
# higher          4.85690953     56.609208
# romantic       -0.64946321      7.513284
# famrel          1.02114078     27.785343
# freetime       -0.62213187     18.924633
# goout          -0.77555014     30.881897
# Dalc            1.13535005     25.826524
# Walc            2.17744240     33.103837
# health          1.37205503     26.615248
# absences        0.27842710     26.768792
# LanguageGrade1 22.76967184    361.281029
# LanguageGrade2 31.38577943    508.142195

# LanguageGrade2 and LanguageGrade1 are the most important variables, distantly followed by
# failures and higher.

####################### Language -- No Grades ################################
m <- sqrt(dim(lang.train.nogrades)[2]-1) # m = sqrt(p)
rf.lang.ng <- randomForest(LanguageFinal ~ ., data = lang.train.nogrades, mtry = m, importance=T)
varImpPlot(rf.lang.ng)
importance(rf.lang.ng)
#                %IncMSE IncNodePurity
# school     -0.78714564      16.09063
# sex         3.47520409      22.44302
# age         3.93266419      57.31046
# address    -1.00557414      19.22775
# famsize    -0.31868880      19.92494
# Pstatus     2.66900039      18.39469
# Medu        3.47387366      58.21036
# Fedu        5.67093111      71.39865
# Mjob        3.79663066      63.07518
# Fjob        0.07499894      62.18018
# reason      0.31808916      61.16692
# nursery     1.48655373      18.24937
# internet   -0.60703649      12.23971
# guardian   -1.54384013      21.51141
# traveltime  5.22897004      50.82984
# studytime   5.43186550      73.14430
# failures   15.20410523     144.88913
# schoolsup   9.19946042      33.74690
# famsup      1.54603637      19.16967
# paid        4.35278619      26.52516
# activities  2.59554491      18.60400
# higher      9.24092250      90.02257
# romantic    0.75144463      20.47306
# famrel     -0.55695312      49.33358
# freetime   -0.83756926      52.96906
# goout       0.22646015      64.49386
# Dalc        3.10333441      51.75837
# Walc        3.73826703      67.73901
# health      3.02677346      67.24434
# absences    1.40877891      69.70327

# Failures, higher and studytime seem to be the top three most important

######################### Math -- With Grades ################################
m <- sqrt(dim(math.train)[2]-1) # m = sqrt(p)
rf.math.g <- randomForest(MathFinal ~ ., data = math.train, mtry = m, importance=T)
varImpPlot(rf.math.g)
importance(rf.math.g)
#                %IncMSE IncNodePurity
# school      0.22733963      11.10591
# sex         1.48664112      29.53833
# age        -1.14223415      54.57679
# address     0.49754103      20.50009
# famsize     1.73217668      23.23476
# Pstatus     1.99957703      12.35883
# Medu        2.07037474      52.85804
# Fedu        1.06401461      49.75975
# Mjob        2.57050709     110.68861
# Fjob        0.84251490      69.26082
# reason      3.15102734      81.54372
# nursery     0.09207561      13.82005
# internet   -2.01095924      15.49017
# guardian    0.93815885      30.56752
# traveltime  2.41433281      50.87225
# studytime   0.55840271      44.35759
# failures    7.10316735     169.67907
# schoolsup   4.94864489      19.15498
# famsup     -1.59960987      16.94260
# paid       -0.09950636      30.13613
# activities -0.35009502      14.41845
# higher      1.80225066      33.23903
# romantic    1.08135243      29.18607
# famrel     -0.62643370      34.64898
# freetime   -0.52532862      41.45132
# goout       4.28382366     132.40827
# Dalc       -0.22775812      24.11861
# Walc        2.46896321      48.74081
# health      1.28408420      55.67213
# absences   15.51123022     247.15136
# MathGrade1 22.76019891     879.08757
# MathGrade2 29.23517810    1292.24930

# MathGrade2, MathGrade1, absences, and failures seem to be the most important variables.

######################### Math -- No Grades ##################################
m <- sqrt(dim(math.train.nogrades)[2]-1) # m = sqrt(p)
rf.math.ng <- randomForest(MathFinal ~ ., data = math.train.nogrades, mtry = m, importance=T)
varImpPlot(rf.math.ng)
importance(rf.math.ng)
# %IncMSE IncNodePurity
# school      0.3787199      22.77611
# sex         4.4096630      85.08245
# age         4.0601768     144.84550
# address     2.3558673      53.32196
# famsize     0.9417227      52.24568
# Pstatus     2.5276843      30.34588
# Medu        3.7310696     121.68558
# Fedu        2.9385644     136.85718
# Mjob        4.2934844     232.66934
# Fjob        0.1618676     172.27464
# reason      2.9615574     174.45814
# nursery    -0.4952071      35.79598
# internet    0.8285422      42.23760
# guardian    3.1192562      79.26418
# traveltime  4.8881461     116.94274
# studytime   1.1383920     119.68458
# failures   15.3434580     321.69242
# schoolsup   4.4652159      48.41753
# famsup      0.3238481      50.91845
# paid        2.3535201      56.76276
# activities -1.0667772      43.53881
# higher      4.1054506      79.24504
# romantic    2.4853550      80.07061
# famrel     -0.5755257      97.73538
# freetime    0.2766296     115.36794
# goout       8.5480396     266.35705
# Dalc       -1.2136655      53.27159
# Walc       -1.3198955     116.19177
# health      0.6878164     133.63936
# absences    9.7753207     318.73981

# Failures, absences, goout, and Mjob seem to be the most important variables.

##############################################################################
#                                                                            #
#                Determining Best Model with Validation                      #
#                                                                            #
##############################################################################

############################# Linear MSEs ####################################
# Language with grades-Linear
preds.lg <- predict(lm.lang.withgrades, newdata = lang.valid)
MSE.lg <- sum((lang.valid$LanguageFinal - preds.lg)^2)

# Language without grades-Linear
preds.lng <- predict(lm.lang.withoutgrades, newdata = lang.valid.nogrades)
MSE.lng <- sum((lang.valid.nogrades$LanguageFinal - preds.lng)^2)

# Math with Grades-Linear
preds.mg <- predict(lm.math.withgrades, newdata = math.valid)
MSE.mg <- sum((math.valid$MathFinal - preds.mg)^2)

# Math without Grades- linear
preds.mng <- predict(lm.math.withoutgrades, newdata=math.valid.nogrades)
MSE.mng <- sum((math.valid.nogrades$MathFinal - preds.mng)^2)

######################## Decision Tree MSEs #################################
# Language with grades-Decision Tree
preds.lg.tree <- predict(tree.lg, newdata = lang.valid)
MSE.lg.tree <- sum((lang.valid$LanguageFinal - preds.lg.tree)^2)

# Language without grades-Decision Tree
preds.lng.tree <- predict(tree.lng, newdata = lang.valid.nogrades)
MSE.lng.tree <- sum((lang.valid.nogrades$LanguageFinal - preds.lng.tree)^2)

# Math with Grades-Decision Tree
preds.mg.tree <- predict(tree.mg, newdata = math.valid)
MSE.mg.tree <- sum((math.valid$MathFinal - preds.mg.tree)^2)

# Math without Grades- Decision Tree
preds.mng.tree <- predict(tree.mng, newdata=math.valid.nogrades)
MSE.mng.tree <- sum((math.valid.nogrades$MathFinal - preds.mng.tree)^2)

######################## Bagging MSEs #################################
# Language with grades-Bagging
preds.lg.bag <- predict(bag.lang.g, newdata = lang.valid)
MSE.lg.bag <- sum((lang.valid$LanguageFinal - preds.lg.bag)^2)

# Language without grades-Bagging
preds.lng.bag <- predict(bag.lang.ng, newdata = lang.valid.nogrades)
MSE.lng.bag <- sum((lang.valid.nogrades$LanguageFinal - preds.lng.bag)^2)

# Math with Grades-Bagging
preds.mg.bag <- predict(bag.math.g, newdata = math.valid)
MSE.mg.bag <- sum((math.valid$MathFinal - preds.mg.bag)^2)

# Math without Grades- Bagging
preds.mng.bag <- predict(bag.math.ng, newdata=math.valid.nogrades)
MSE.mng.bag <- sum((math.valid.nogrades$MathFinal - preds.mng.bag)^2)

######################## Random Forest MSEs ############################

# Language with grades-RF
preds.lg.rf <- predict(rf.lang.g, newdata = lang.valid)
MSE.lg.rf <- sum((lang.valid$LanguageFinal - preds.lg.rf)^2)

# Language without grades-RF
preds.lng.rf <- predict(rf.lang.ng, newdata = lang.valid.nogrades)
MSE.lng.rf <- sum((lang.valid.nogrades$LanguageFinal - preds.lng.rf)^2)

# Math with Grades-RF
preds.mg.rf <- predict(rf.math.g, newdata = math.valid)
MSE.mg.rf <- sum((math.valid$MathFinal - preds.mg.rf)^2)

# Math without Grades- RF
preds.mng.rf <- predict(rf.math.ng, newdata=math.valid.nogrades)
MSE.mng.rf <- sum((math.valid.nogrades$MathFinal - preds.mng.rf)^2)

# Creating a Table of MSEs
linear <- c(MSE.lg, MSE.lng, MSE.mg, MSE.mng)
rf <- c(MSE.lg.rf, MSE.lng.rf, MSE.mg.rf, MSE.mng.rf)
bag <- c(MSE.lg.bag, MSE.lng.bag, MSE.mg.bag, MSE.mng.bag)
tree <- c(MSE.lg.tree, MSE.lng.tree, MSE.mg.tree, MSE.mng.tree)

MSEs <- as.data.frame(cbind(linear, tree, bag, rf))
rownames(MSEs) <- c("Language", "Language-No Grades", "Math", "Math-No Grades")
colnames(MSEs) <- c("OLS", "Decision Tree", "Bagging", "Random Forest")
MSEs

#                          OLS Decision Tree   Bagging Random Forest
# Language            443.7734      330.6630  435.7170      528.7536
# Language-No Grades 1440.4379     1800.0251 1297.3807     1279.9412
# Math                972.1921      810.4962  606.7125      991.7681
# Math-No Grades     4209.4093     4749.7283 3193.2623     3410.8108

