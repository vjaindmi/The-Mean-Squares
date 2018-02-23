
setwd ("C:\\Work\\Lochbridge work\\Others\\Hackathon\\Diabetes model/")

library(caret)


#---------------------------------------------Convert SAS format to CSV format-----------------------------------------------#


library(Hmisc)

data <- sasxport.get("LLCP2016.XPT")

# write.csv (data, "LLCP2016.csv", row.names = F)


#----------------------------------------------Read and Preprocess Raw Dataset-----------------------------------------------#

# Read raw data

# survey_data <- read.csv ("LLCP2016.csv")

survey_data <- data

# Features manually selected for Diabetes prediction model

# myvars <- c("x.ageg5yr", "htin4", "weight2", "sex", "drnkany5", "smokday2", "exerany2", "sleptim1", "diabete3")


myvars <- c("x.ageg5yr", "htin4", "weight2", "sex", "drnkany5", "smokday2", "exerany2", "sleptim1", 
            "physhlth", "checkup1", "chckidny", "diabete3")




# Subset raw dataset with selected features

survey_data_subset <- survey_data [myvars]

survey_data_subset <- na.omit (survey_data_subset)


#-----------------------------------------------------diabete3---------------------------------------------------------#


# Select diabete3 column having "1: Yes" or "3: No"

survey_data_subset <- subset (survey_data_subset, diabete3 == 1 | diabete3 == 3)


# Update diabete3 column - 1 for "Yes" and 0 for "No" - 3 is No here

survey_data_subset$diabete3 <- ifelse (survey_data_subset$diabete3 == 3, 0, 1)



#-----------------------------------------------------x.ageg5yr---------------------------------------------------------#

survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 1, 21, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 2, 27, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 3, 32, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 4, 37, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 5, 42, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 6, 47, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 7, 52, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 8, 57, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 9, 62, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 10, 67, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 11, 72, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 12, 77, survey_data_subset$x.ageg5yr)
survey_data_subset$x.ageg5yr <- ifelse (survey_data_subset$x.ageg5yr == 13, 90, survey_data_subset$x.ageg5yr)


survey_data_subset <- survey_data_subset [which (survey_data_subset$x.ageg5yr != 14),]



#-----------------------------------------------------htin4---------------------------------------------------------#

survey_data_subset <- survey_data_subset [which (survey_data_subset$htin4 >= 36 & survey_data_subset$htin4 <= 95),]

survey_data_subset$htin4 <- survey_data_subset$htin4 * 2.54



#-----------------------------------------------------weight2---------------------------------------------------------#

survey_data_subset <- survey_data_subset [which (survey_data_subset$weight2 >= 50 & survey_data_subset$weight2 <= 500),]

survey_data_subset$weight2 <- survey_data_subset$weight2 * 0.453592



#-----------------------------------------------------sex---------------------------------------------------------#

survey_data_subset <- survey_data_subset [which (survey_data_subset$sex == 1 | survey_data_subset$sex == 2),]



#-----------------------------------------------------drnkany5---------------------------------------------------------#

# Select drnkany5 column having 1: Yes or 2: No

survey_data_subset <- subset (survey_data_subset, drnkany5 == 1 | drnkany5 == 2)

#survey_data_subset$drnkany5 <- ifelse (survey_data_subset$drnkany5 == 1, 0, survey_data_subset$drnkany5)

#survey_data_subset$drnkany5 <- ifelse (survey_data_subset$drnkany5 == 2, 1, survey_data_subset$drnkany5)



#-----------------------------------------------------smokday2---------------------------------------------------------#

# Select smokday2 column having 1: Everyday or 2: Some days or 3: Not at all

survey_data_subset <- subset (survey_data_subset, smokday2 == 1 | smokday2 == 2 | smokday2 == 3)


# Update smokday2 column - 0 for 3 - No smoke

survey_data_subset$smokday2 <- ifelse (survey_data_subset$smokday2 == 3, 0, survey_data_subset$smokday2)



#-----------------------------------------------------exerany2---------------------------------------------------------#


# Select exerany2 column 1: Yes or 2: No

survey_data_subset <- subset (survey_data_subset, exerany2 == 1 | exerany2 == 2)



#-----------------------------------------------------sleptim1---------------------------------------------------------#


# Select sleptim1 column having 1:24

survey_data_subset <- subset (survey_data_subset, sleptim1 <= 24 & sleptim1 >= 1)


#-----------------------------------------------------physhlth---------------------------------------------------------#

# Select physhlth column having 1:24

survey_data_subset$physhlth <- ifelse (survey_data_subset$physhlth == 88, 0, survey_data_subset$physhlth)

survey_data_subset <- subset (survey_data_subset, physhlth <= 30 & physhlth >= 0)


#-----------------------------------------------------checkup1---------------------------------------------------------#


# Select checkup1 column having 1: Past 1 Year, 2: 2 Year etc. Select 1, 2, 3, 4, 8

survey_data_subset <- subset (survey_data_subset, checkup1 == 1 | checkup1 == 2 | checkup1 == 3 | checkup1 == 4 | checkup1 == 8)


#-----------------------------------------------------chckidny---------------------------------------------------------#


# Select chckidny column having 1: Yes, 2: No

survey_data_subset <- subset (survey_data_subset, chckidny == 1 | chckidny == 2)

# table (survey_data_subset$diabete3)


#-----------------------------------------------------Reduce Imbalance---------------------------------------------------------#


survey_data_subset_diabetec <- subset (survey_data_subset, diabete3 == 1)

survey_data_subset_non_diabetec <- subset (survey_data_subset, diabete3 == 0)

# survey_data_subset_non_diabetec_subset <- survey_data_subset_non_diabetec [sample(nrow(survey_data_subset_non_diabetec), 29053), ]

survey_data_subset_non_diabetec_subset <- survey_data_subset_non_diabetec [sample(nrow(survey_data_subset_non_diabetec),  27739), ]

survey_data_subset3 <- rbind (survey_data_subset_diabetec, survey_data_subset_non_diabetec_subset)

# dim (survey_data_subset3)

# View (survey_data_subset3)

# table (survey_data_subset3$diabete3)



#--------------------------------------------Select Numeric and Factor Variables-----------------------------------------------#


# survey_data_subset <- read.csv ("survey_data_subset_subset1.csv")


survey_data_subset <- survey_data_subset3


survey_data_subset$x.ageg5yr <- as.numeric (survey_data_subset$x.ageg5yr)
survey_data_subset$htin4 <- as.numeric (survey_data_subset$htin4)
survey_data_subset$weight2 <- as.numeric (survey_data_subset$weight2)
survey_data_subset$sex <- as.factor (survey_data_subset$sex)
survey_data_subset$drnkany5 <- as.factor (survey_data_subset$drnkany5)
survey_data_subset$smokday2 <- as.factor (survey_data_subset$smokday2)
survey_data_subset$exerany2 <- as.factor (survey_data_subset$exerany2)
survey_data_subset$sleptim1 <- as.numeric (survey_data_subset$sleptim1)
survey_data_subset$physhlth <- as.numeric (survey_data_subset$physhlth)
survey_data_subset$checkup1 <- as.numeric (survey_data_subset$checkup1)
survey_data_subset$chckidny <- as.factor (survey_data_subset$chckidny)


survey_data_subset$diabete3 <- as.factor (survey_data_subset$diabete3)



# write.csv (survey_data_subset, "survey_data_subset.csv", row.names = F)



#----------------------------------------------------Rename Variables----------------------------------------------------------#


#colnames (survey_data_subset) <- c("Age", "Height(CM)", "Weight(Kg)", "Sex(M_1_F_2)", "Alcohol(Yes_1_No_2)", 
#                                   "Smoke(No_0_Everyday_1_SomeDayes_2)", "Exercise(Yes_1_No_2)", "SleepHours", "diabete3")




#--------------------------------------------Logistic Model on Complete Dataset------------------------------------------------#


# model <- glm (diabete3 ~ ., data = survey_data_subset, family = "binomial")


# anova (model, test="Chisq")

# predict_adjusted = ifelse (model$fitted.values >= 0.5, 1, 0)

# table (predict_adjusted)

# confusionMatrix (predict_adjusted, survey_data_subset$diabete3)


#              Reference
# Prediction    0    1
#          0 19921  8555
#          1  9132  20498
# Accuracy: 0.69



#---------------------------------------Logistic Model Using Training and Test Dataset----------------------------------------#

# survey_data_subset <- read.csv ("survey_data_subset.csv")


# Divide dataset into 70% training and 30% Test

# set the seed to make your partition reproductible
set.seed(125)

# 70% of the sample size
smp_size <- floor(0.80 * nrow (survey_data_subset))

# Random sampling of dataset
train_ind <- sample(seq_len(nrow(survey_data_subset)), size = smp_size)

train <- survey_data_subset [train_ind, ]
test <- survey_data_subset [-train_ind, ]

dim (train)
dim (test)


model <- glm (diabete3 ~ ., data = train, family = "binomial")

# library(ResourceSelection)
# Hosmer-Lemeshow goodness of fit test
# hoslem.test(train$DIABETE3, fitted(model), g=10)

test_predict <- predict (model, newdata = test, type="response")

test_predict_adjusted = ifelse (test_predict >= 0.5, 1, 0)

table (test_predict_adjusted)

confusionMatrix (test_predict_adjusted, test$diabete3)

#              Reference
#Prediction    0    1
#          0 3889  1524
#          1 1658  4025



#----------------------------------------------------Variable Importance----------------------------------------------------#


#Variable Importance based on Absolute value of the t-statistic
var_imp <- varImp (model)
var_imp <- var_imp [order(-var_imp$Overall), , drop = FALSE]
#colnames(var_imp) <- c("Feature", "Importance")
#var_imp1 <- var_imp[0:10,]
#barplot(t(as.matrix(var_imp)), main="Variable Importance", xlab="Features", ylab = "Importance (Abs_t_statistic)")



#----------------------------------------------------View as a Webpage---------------------------------------------------------#

library (predictshine)

predictshine (model, 
              page_title = 'Diabetes Risk Prediction',
              main = 'Probability of Diabetes',
              description = p('Probability of Diabetes from body and lifestyle features.'))




