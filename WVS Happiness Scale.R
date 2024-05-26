
###setwd

setwd("E:/IIT-D/Projects/WVS/Happiness Predictors and Strategies/Understanding Wellbeing in India - Psychometric Evaluation of a WVS-Derived Scale/Analysis")


###Libraries


#install.packages("lsr")


library(tidyverse)
library(dplyr)
library(broom)
library(MASS)
library(magrittr)
library(ez)
library(stargazer)
library(readxl)
library(lavaan)
library(corrplot)
library(psych)
library(REdaS)
library(stats)
library(mice)
library(moments)
library(lsr)


#----------------------------------------------------


###WVS India Data 

WVS7_Ind_Data <- read_excel("WVS_Ind_Coded.xlsx")

dim(WVS7_Ind_Data)
head(WVS7_Ind_Data)
summary(WVS7_Ind_Data)

#View(WVS7_Ind_Data)


WVS7_Ind <- WVS7_Ind_Data

dim(WVS7_Ind)
head(WVS7_Ind)
summary(WVS7_Ind)


#----------------------------------------------------


###SWB Measurement


#------------------


##Feeling of Happiness

FoH <- WVS7_Ind$`Q46: Feeling of happiness`

unique(FoH)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q46: Feeling of happiness`[WVS7_Ind$`Q46: Feeling of happiness` == -1] <- NA

FoH <- WVS7_Ind$`Q46: Feeling of happiness`

unique(FoH)


#Recoding the Variable (FoH)

FoH_R <- FoH %>%
  recode(`4` = 1, `3` = 2, `2` = 3, `1` = 4)

table(FoH, FoH_R)

FoH <- FoH %>%
  recode(`4` = 1, `3` = 2, `2` = 3, `1` = 4)


#Recoding the variable (FoH) in the dataset

WVS7_Ind$`Q46: Feeling of happiness` <- WVS7_Ind$`Q46: Feeling of happiness` %>%
  recode(`4` = 1, `3` = 2, `2` = 3, `1` = 4)



#------------------


##Life Satisfaction 

LS <- WVS7_Ind$`Q49: Satisfaction with your life`

unique(LS)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q49: Satisfaction with your life`[WVS7_Ind$`Q49: Satisfaction with your life` == -1] <- NA

LS <- WVS7_Ind$`Q49: Satisfaction with your life`

unique(LS)



#------------------


##State of Health

Hlt <- WVS7_Ind$`Q47: State of health (subjective)`

unique(Hlt)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q47: State of health (subjective)`[WVS7_Ind$`Q47: State of health (subjective)` == -1] <- NA

Hlt <- WVS7_Ind$`Q47: State of health (subjective)`

unique(Hlt)


#Recoding the Variable (Hlt)

Hlt_R <- Hlt %>%
  recode(`5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5)

table(Hlt, Hlt_R)

Hlt <- Hlt %>%
  recode(`5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5)


#Recoding the variable (Hlt) in the dataset

WVS7_Ind$`Q47: State of health (subjective)` <- WVS7_Ind$`Q47: State of health (subjective)` %>%
  recode(`5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5)



#------------------


##Autonomy 


Aut <- WVS7_Ind$`Q48: How much freedom of choice and control`

unique(Aut)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q48: How much freedom of choice and control`[WVS7_Ind$`Q48: How much freedom of choice and control` == -1] <- NA

Aut <- WVS7_Ind$`Q48: How much freedom of choice and control`

unique(Aut)


#------------------


##Financial Satisfaction


FS <- WVS7_Ind$`Q50: Satisfaction with financial situation of household`

unique(FS)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q50: Satisfaction with financial situation of household`[WVS7_Ind$`Q50: Satisfaction with financial situation of household` == -1] <- NA

FS <- WVS7_Ind$`Q50: Satisfaction with financial situation of household`

unique(FS)



#----------------------------------------------------

#----------------------------------------------------


###Socio-demographic Variables


#------------------


##Age

Age <- WVS7_Ind$`X003R2: Age recoded (3 intervals)`

unique(Age)

# Recode '-1' as NA (missing value)

WVS7_Ind$`X003R2: Age recoded (3 intervals)`[WVS7_Ind$`X003R2: Age recoded (3 intervals)` == -1] <- NA

Age <- WVS7_Ind$`X003R2: Age recoded (3 intervals)`

unique(Age)


Age_total <- WVS7_Ind$`Q262: Age`
unique(Age_total)


#------------------


##Sex

Sex <- WVS7_Ind$`Q260: Sex`

unique(Sex)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q260: Sex`[WVS7_Ind$`Q260: Sex` == -1] <- NA

Sex <- WVS7_Ind$`Q260: Sex`

unique(Sex)



#------------------


##Educational Level


Ed <- WVS7_Ind$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`

unique(Ed)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`[WVS7_Ind$`Q275R: Highest educational level: Respondent (recoded into 3 groups)` == -1] <- NA

Ed <- WVS7_Ind$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`

unique(Ed)



#------------------


##Social class (SC)

SC <- WVS7_Ind$`Q287: Social class (subjective)`

unique(SC)



# Recode '-1' as NA (missing value)


WVS7_Ind$`Q287: Social class (subjective)`[WVS7_Ind$`Q287: Social class (subjective)` == -1] <- NA

SC <- WVS7_Ind$`Q287: Social class (subjective)`

unique(SC)


#Recoding the Variable (SC)


SC_R <- SC %>%
  recode(`5` = 1,`4` = 2, `3` = 3, `2` = 4, `1` = 5)

table(SC, SC_R)

SC <- SC %>%
  recode(`5` = 1,`4` = 2, `3` = 3, `2` = 4, `1` = 5)


#Recoding the Variable (SC) in the dataset

WVS7_Ind$`Q287: Social class (subjective)` <- WVS7_Ind$`Q287: Social class (subjective)` %>%
  recode(`5` = 1,`4` = 2, `3` = 3, `2` = 4, `1` = 5)


#----------------------------------------------------

#----------------------------------------------------


###D-Needs Classification 


#------------------


##D-Needs Variables 


D_Need1 <- WVS7_Ind$`Q51: Frequency you/family (last 12 month): Gone without enough food to eat`
unique(D_Need1)

D_Need2 <- WVS7_Ind$`Q52: Frequency you/family (last 12 month): Felt unsafe from crime in your own home`
unique(D_Need2)

D_Need3 <- WVS7_Ind$`Q53: Frequency you/family (last 12 month): Gone without needed medicine or treatment that you needed`
unique(D_Need3)

D_Need4 <- WVS7_Ind$`Q54: Frequency you/family (last 12 month): Gone without a cash income`
unique(D_Need4)

D_Need5 <- WVS7_Ind$`Q55: In the last 12 month, how often have you or your family: Gone without a safe shelter over your head`
unique(D_Need5)


# Recode '-1' as NA (missing value)

WVS7_Ind$`Q51: Frequency you/family (last 12 month): Gone without enough food to eat`[WVS7_Ind$`Q51: Frequency you/family (last 12 month): Gone without enough food to eat` == -1] <- NA
WVS7_Ind$`Q52: Frequency you/family (last 12 month): Felt unsafe from crime in your own home`[WVS7_Ind$`Q52: Frequency you/family (last 12 month): Felt unsafe from crime in your own home` == -1] <- NA
WVS7_Ind$`Q53: Frequency you/family (last 12 month): Gone without needed medicine or treatment that you needed`[WVS7_Ind$`Q53: Frequency you/family (last 12 month): Gone without needed medicine or treatment that you needed` == -1] <- NA
WVS7_Ind$`Q54: Frequency you/family (last 12 month): Gone without a cash income`[WVS7_Ind$`Q54: Frequency you/family (last 12 month): Gone without a cash income` == -1] <- NA
WVS7_Ind$`Q55: In the last 12 month, how often have you or your family: Gone without a safe shelter over your head`[WVS7_Ind$`Q55: In the last 12 month, how often have you or your family: Gone without a safe shelter over your head` == -1] <- NA


D_Need1 <- WVS7_Ind$`Q51: Frequency you/family (last 12 month): Gone without enough food to eat`
D_Need2 <- WVS7_Ind$`Q52: Frequency you/family (last 12 month): Felt unsafe from crime in your own home`
D_Need3 <- WVS7_Ind$`Q53: Frequency you/family (last 12 month): Gone without needed medicine or treatment that you needed`
D_Need4 <- WVS7_Ind$`Q54: Frequency you/family (last 12 month): Gone without a cash income`
D_Need5 <- WVS7_Ind$`Q55: In the last 12 month, how often have you or your family: Gone without a safe shelter over your head`


unique(D_Need1)
unique(D_Need2)
unique(D_Need3)
unique(D_Need4)
unique(D_Need5)



#----------------------------------------------------

#----------------------------------------------------



###Create a Data frame for the Analysis


#Load the variables to equalize the length 


length(FoH)
length(LS)
length(Hlt)
length(Aut)
length(FS)
length(Age)
length(Sex)
length(Ed)
length(SC)
length(D_Need1)
length(D_Need2)
length(D_Need3)
length(D_Need4)
length(D_Need5)


FoH <- WVS7_Ind$`Q46: Feeling of happiness`
LS <- WVS7_Ind$`Q49: Satisfaction with your life`
Hlt <- WVS7_Ind$`Q47: State of health (subjective)`
Aut <- WVS7_Ind$`Q48: How much freedom of choice and control`
FS <- WVS7_Ind$`Q50: Satisfaction with financial situation of household`
Age <- WVS7_Ind$`X003R2: Age recoded (3 intervals)`
Sex <- WVS7_Ind$`Q260: Sex`
Ed <- WVS7_Ind$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`
SC <- WVS7_Ind$`Q287: Social class (subjective)`
D_Need1 <- WVS7_Ind$`Q51: Frequency you/family (last 12 month): Gone without enough food to eat`
D_Need2 <- WVS7_Ind$`Q52: Frequency you/family (last 12 month): Felt unsafe from crime in your own home`
D_Need3 <- WVS7_Ind$`Q53: Frequency you/family (last 12 month): Gone without needed medicine or treatment that you needed`
D_Need4 <- WVS7_Ind$`Q54: Frequency you/family (last 12 month): Gone without a cash income`
D_Need5 <- WVS7_Ind$`Q55: In the last 12 month, how often have you or your family: Gone without a safe shelter over your head`


length(FoH)
length(LS)
length(Hlt)
length(Aut)
length(FS)
length(Age)
length(Sex)
length(Ed)
length(SC)
length(D_Need1)
length(D_Need2)
length(D_Need3)
length(D_Need4)
length(D_Need5)


#------------------

#Create the final Dataframe (master)

master <- data.frame(FoH, LS, Hlt, Aut, FS, Age, Age_total, Sex, Ed, SC, D_Need1, D_Need2, D_Need3, D_Need4, D_Need5)

dim(master)


#View(master)

write.csv(master, file = "master.csv")



#----------------------------------------------------


###Data scrubbing


##missing

percentmissing = function (x){ sum(is.na(x))/length(x) * 100}

missing1 = apply(master, 1, percentmissing)

table(missing1)


##exclude the participant missing too much data

replacepeople = subset(master, missing1 <= 5)

dim(replacepeople)

missing2 = apply(replacepeople, 1, percentmissing)

table(missing2)


##make sure the columns aren't missing too much

missing3 = apply(replacepeople, 2, percentmissing)

table(missing3)


##replace away!

tempnomiss = mice(replacepeople)

nomiss = complete(tempnomiss, 1)

summary(nomiss)

dim(nomiss)


nomiss <- replacepeople #Since there are no missing values


#number of missing values

total_missing <- sum(is.na(nomiss))
print(total_missing)


##outliers


# Cutoff score using Chi-square

colnames(nomiss)

cutoff = qchisq(1-.001, ncol(nomiss[, -c(6:15)]))

cutoff #Cutoff score

ncol(nomiss[, -c(6:15)]) #Degrees of freedom

# Exclude columns 10:13 from the data

nomiss_subset <- nomiss[, -c(6:15)]

dim(nomiss_subset)

# Compute the mean of each column excluding columns 10:13

means_nomiss_subset <- colMeans(nomiss_subset)

# Compute the covariance matrix using pairwise complete observations

covariance_nomiss_subset <- cov(nomiss_subset, use = "pairwise.complete.obs")

# Compute Mahalanobis distance

mahal <- mahalanobis(nomiss_subset, means_nomiss_subset, covariance_nomiss_subset)

summary(mahal < cutoff)


# check mahal score to the dataset

nomiss$mahal <- round(mahal, 3)

View(nomiss)

dim(nomiss)

nomiss <- subset(nomiss, select = -mahal)

dim(nomiss)


##exclude outliers

noout = subset(nomiss, mahal < cutoff)

dim(noout)



#Save the polished data 

write.csv(noout, file = "scrubbed_data.csv")


#----------------------------------------------------


###Assumptions


#Additivity 

correl = cor(noout[, -c(6:9)])
symnum(correl)

final = noout 


#Assumption set up

random = rchisq(nrow(final), 7)

fake = lm(random~., data = final)

standardized = rstudent(fake)

fitted = scale(fake$fitted.values)




#Normality

skewness(final[ -c(6:9)])
kurtosis(final[ -c(6:9)])

hist(standardized)


#linearity

qqnorm(standardized)
abline(0,1)
abline(v = -2)
abline(v = 2)

#homogeneity & homoscedasticity

plot(fitted, standardized)
abline(0,0)
abline(v = 0)
abline(v = -3)
abline(v = 3)


#Save the final file

fdata = final  

write.csv(fdata, file = "final_data.csv")

#----------------------------------------------------


###Analyses


# Check the structure of the dataset

dim(fdata)
str(fdata)

#------------------



##Analysis 1 (Correlation)


#Create the correlation matrix

correlation_matrix <- cor(fdata)


#Print the correlation matrix using stargazer

stargazer(correlation_matrix, type = "text")


#Save the correlation matrix to a CSV file

write.csv(correlation_matrix, file = "correlation_matrix.csv")


#Create a visualization of the correlation matrix

corrplot(correlation_matrix, method = "circle", type = "lower", tl.cex = 0.7, tl.col = "black", diag = FALSE)


#Save the visualization as a PNG file

png("correlation_matrix.png", width = 800, height = 800, res = 300)

corrplot(correlation_matrix, method = "circle", type = "lower", tl.cex = 0.7, tl.col = "black", diag = FALSE)

dev.off()


#------------------


##Creating Dataframes for EFA and CFA


#Randomly split the dataset into two subsets

"
set.seed(123)  
EFA.data_indices <- sample(1:nrow(fdata), size = round(nrow(fdata) * 1/3))
  
EFA.data <- fdata[EFA.data_indices, ]
CFA.data <- fdata[-EFA.data_indices, ]

dim(EFA.data)
dim(CFA.data)

#Save the subsets as CSV files

write.csv(EFA.data, file = "EFA.data.csv", row.names = FALSE)
write.csv(CFA.data, file = "CFA.data.csv", row.names = FALSE)

#Load the EFA and CFA files

EFA.data <- read.csv("EFA.data.csv")
CFA.data <- read.csv("CFA.data.csv")


"


dim(EFA.data)
dim(CFA.data)


#------------------


##Analysis 1 (Exploratory Factor Analysis - 9 items)


#Bartletts test of sphericity 

cortest.bartlett(EFA.data[ -c(6:14)])
bart_spher(EFA.data[ -c(6:14)])

#Kaiser-Meyer-Olkin measure

KMO(EFA.data[ -c(6:14)])


#Parallel Analysis

nofactors = fa.parallel(EFA.data[ -c(6:14)], fm = "ml", fa = "fa")
sum(nofactors$fa.values > 1.0) #Old Kaiser Criterion
sum(nofactors$fa.values > 0.7) #New Kaiser Criterion


#Principal Component Analysis (PCA)

pca_result <- prcomp(EFA.data[ -c(6:14)], scale = TRUE)

summary(pca_result)

pca_scores <- pca_result$x
head(pca_scores)

loadings <- pca_result$rotation
head(loadings)

# Biplot

biplot(pca_result)

# Scree plot

plot(pca_result, type="l", main="Scree Plot")



#Simple Structure with a two factor model (maximum likelihood)

EFA_TwoFactor_ML = fa(EFA.data[ -c(6:14)], nfactors = 2, rotate = "oblimin", fm = "ml")
EFA_TwoFactor_ML
fa.diagram(EFA_TwoFactor_ML)


#Simple Structure with a two factor model (Principal Axis)

EFA_TwoFactor_PA = fa(EFA.data[ -c(6:14)], nfactors = 2, rotate = "oblimin", fm = "pa")
EFA_TwoFactor_PA
fa.diagram(EFA_TwoFactor_PA)


#Reliabililty 

colnames(EFA.data)

factor1 = c(1, 3)

factor2 = c(2, 4, 5)

psych::alpha(EFA.data[ , factor1])
psych::alpha(EFA.data[ , factor2])
psych::alpha(EFA.data[ , 1:5])


SB_EFA.cor <- cor(EFA.data$FoH, EFA.data$Hlt, method = "spearman")
SB_EFA.rel <- (2 * SB_EFA.cor) / (1 + SB_EFA.cor)
print(SB_EFA.rel) # Spearman-Brown reliability coefficient


#------------------


##Analysis 3 (Confirmatory Factor Analysis)


#Bartletts test of sphericity 

cortest.bartlett(CFA.data[ -c(6:14)])
bart_spher(CFA.data[ -c(6:14)])

#Kaiser-Meyer-Olkin measure

KMO(CFA.data[ -c(6:14)])


##Specifying the Model (Two-factor)

colnames(CFA.data)

CFA.TwoFactor <- '
    SWB1 =~ FoH + Hlt 
    SWB2 =~ LS + Aut + FS 
'

##CFA (Two-factor model)

CFA.TwoFactor_model = cfa(CFA.TwoFactor, data = CFA.data[ -c(6:14)])
summary(CFA.TwoFactor_model, fit.measures = T, rsquare = T, standardized = T)
fitmeasures(CFA.TwoFactor_model)
parameterestimates(CFA.TwoFactor_model)


##Specifying the Model (Uni-factor)

CFA.Sin_Hap <- '
    SWB =~ FoH +LS + Aut + FS + Hlt
'

##CFA (Uni-factor model)

CFA.Sin_model = cfa(CFA.Sin_Hap, data = CFA.data[ -c(6:14)])
summary(CFA.Sin_model, fit.measures = T, rsquare = T, standardized = T)
fitmeasures(CFA.Sin_model)
parameterestimates(CFA.Sin_model)


#Reliabililty 

colnames(CFA.data)

factor1 = c(1, 3)

factor2 = c(2, 4, 5)

psych::alpha(CFA.data[ , factor1])
psych::alpha(CFA.data[ , factor2])
psych::alpha(CFA.data[ , 1:5])

SB_CFA.cor <- cor(CFA.data$FoH, CFA.data$Hlt, method = "spearman")
SB_CFA.rel <- (2 * SB_CFA.cor) / (1 + SB_CFA.cor)
print(SB_CFA.rel) # Spearman-Brown reliability coefficient


#------------------


##Including factor scores in to the dataset


fdata_WB <- fdata 

fdata_WB$WB <- fdata$FoH + fdata$LS + fdata$Hlt + fdata$Aut + fdata$FS

fdata_WB$WB1 <- fdata$FoH + fdata$Hlt

fdata_WB$WB2 <- fdata$LS + fdata$Aut + fdata$FS 

str(fdata_WB)
dim(fdata_WB)


#Saving the final file

write.csv(fdata_WB, file = "final_WB_data.csv")


#------------------


##Analysis 4 (D-Needs comparison)


# Load dataset

ttest_data <- fdata_WB
dim(ttest_data)
str(ttest_data)


# Specify the D_Need columns to be summed

D_Need_columns_to_sum <- c("D_Need1", "D_Need2", "D_Need3", "D_Need4", "D_Need5")

# Calculate total score for D_Need by summing the specified columns for each row

ttest_data$D_Need_total <- rowSums(ttest_data[, D_Need_columns_to_sum])
D_Need_total <- ttest_data$D_Need_total

# Get quartiles

D_Need_quartiles <- quantile(ttest_data$D_Need_total, probs = c(0.25, 0.75))


# Split the dataset into two groups based on quartiles

mean(D_Need_total)
D_Need_quartiles[1]
D_Need_quartiles[2]

group1 <- ttest_data[ttest_data$D_Need_total <= D_Need_quartiles[1], ]
group2 <- ttest_data[ttest_data$D_Need_total >= D_Need_quartiles[2], ]
  
dim(group1)
dim(group2)

# Perform t-test between the two groups using the first 5 variables

t_test_WB1 <- t.test(group1$WB1, group2$WB1)
print(t_test_WB1)

t_test_WB2 <- t.test(group1$WB2, group2$WB2)
print(t_test_WB2)


# Effect size 

d_WB1 <- cohensD(group1$WB1, group2$WB1)
d_WB1

d_WB2 <- cohensD(group1$WB2, group2$WB2)
d_WB2

#------------------






