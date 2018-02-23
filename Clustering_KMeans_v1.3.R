columns <- cbind("Age", "Height.CM.", "Weight.Kg.", "SleepHours")
test_input <- cbind(55, 125, 83, 6)
colnames(test_input) <- columns

pred_test <- predict (cl1, newdata = test_input)


setwd ("C:/Users/rraj/Documents/Studies/Hacathon")

library(caret)
library(sqldf)


#---------------------------------------------Convert SAS format to CSV format-----------------------------------------------#


library(Hmisc)

data <- sasxport.get("LLCP2016.XPT")

# write.csv (data, "LLCP2016.csv", row.names = F)


########################Subset Variables of Interest############################################

myvars <- c("x.ageg5yr", "htin4", "weight2", "sex", "drnkany5", "smokday2", "exerany2", "sleptim1", "physhlth", "chckidny", "educa", "income2", "diabete3" , "avedrnk2"
             )

survey_data <- data

survey_data_subset <- survey_data [myvars]

survey_data_subset <- na.omit (survey_data_subset)

survey_data_subset <- subset (survey_data_subset, diabete3 == 3)

######################variable treatment #############################################

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


#-----------------------------------------Income2----------------------------------------#

survey_data_subset <- survey_data_subset [which(survey_data_subset$income2 < 77),]



survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 1, 5000, survey_data_subset$income2)
survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 2, 12500, survey_data_subset$income2)
survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 3, 17500, survey_data_subset$income2)
survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 4, 22500, survey_data_subset$income2)
survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 5, 30000, survey_data_subset$income2)
survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 6, 42500, survey_data_subset$income2)
survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 7, 62500, survey_data_subset$income2)
survey_data_subset$income2 <- ifelse (survey_data_subset$income2 == 8, 75000, survey_data_subset$income2)





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


#-----------------------------------------------------chckidny---------------------------------------------------------#


# Select chckidny column having 1: Yes, 2: No

survey_data_subset <- subset (survey_data_subset, chckidny == 1 | chckidny == 2)



###############################################################################################################################

#--------------------------------------------Select Numeric and Factor Variables-----------------------------------------------#


survey_data_subset$x.ageg5yr <- as.numeric (survey_data_subset$x.ageg5yr)
survey_data_subset$htin4 <- as.numeric (survey_data_subset$htin4)
survey_data_subset$weight2 <- as.numeric (survey_data_subset$weight2)
survey_data_subset$sex <- as.factor (survey_data_subset$sex)
survey_data_subset$drnkany5 <- as.factor (survey_data_subset$drnkany5)
survey_data_subset$smokday2 <- as.factor (survey_data_subset$smokday2)
survey_data_subset$exerany2 <- as.factor (survey_data_subset$exerany2)
survey_data_subset$sleptim1 <- as.numeric (survey_data_subset$sleptim1)
survey_data_subset$physhlth <- as.numeric (survey_data_subset$physhlth)
survey_data_subset$chckidny <- as.factor (survey_data_subset$chckidny)
survey_data_subset$income2 <- as.numeric (survey_data_subset$income2)


survey_data_subset$diabete3 <- as.factor (survey_data_subset$diabete3)

###########################################Column Renaming ##################################################################

colnames (survey_data_subset) <- c("Age", "Height(CM)", "Weight(Kg)", "Sex(M_1_F_2)", "Alcohol(Yes_1_No_2)", 
                                                                  "Smoke(No_0_Everyday_1_SomeDayes_2)", "Exercise(Yes_1_No_2)", "SleepHours", "Heath Not Good(last 30 days)",
                                  "Kidney Diseases", "Education level", "Household Annual Income",  "diabete3" ,"# Alcoholic Drinks")

######################Scale the Clustering variables##############################
survey_data_subset_Scaled <- scale(subset(survey_data_subset, select= c(1:3,8,12), center=TRUE, scale=TRUE))

################WSS Plot to determine # of Clusters################################
wss <- (nrow(survey_data_subset_Scaled)-1)*sum(apply(survey_data_subset_Scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(survey_data_subset_Scaled, centers=i)$withinss)

options(scipen=999)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

##################K Means Clustering#####################

cl1 = kcca(survey_data_subset_Scaled[, c(1:5)], k=6, kccaFamily("kmeans"))
cluster <- predict(cl1)

data_non_diabetec_cluster <- cbind (survey_data_subset, cluster)
####calculate BMI ######

############################Profiling Clusters Variables################################################

Age_Summary <-sqldf("select cluster , count(*), min(Age) , max(Age) , avg(Age) from data_non_diabetec_cluster group by cluster")

Height_Summary <-sqldf("select cluster , count(*), min([Height(CM)]) , max([Height(CM)]) , avg([Height(CM)]) from data_non_diabetec_cluster group by cluster")

Weight_Summary <-sqldf("select cluster , count(*), min([Weight(Kg)]) , max([Weight(Kg)]) , avg([Weight(Kg)]) from data_non_diabetec_cluster group by cluster")

Sleep_Summary <-sqldf("select cluster , count(*), min(SleepHours) , max(SleepHours) , avg(SleepHours) from data_non_diabetec_cluster group by cluster")

Income_Summary <- sqldf("select cluster , count(*), min([Household Annual Income]) , max([Household Annual Income]) ,
                        avg([Household Annual Income]) from data_non_diabetec_cluster group by cluster")

#######save data set for Shiny APp#############
write.csv(survey_data_subset , file= "survey_data_subset_Shiny.csv")


