###############################
#                             #
#     Financial Analytics:    #
#     Variable Grouping &     #
#   Selection for Scorecard   #
#                             #
#        Dr Aric LaBarr       #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("gmodels")
install.packages("vcd")
install.packages("smbinning")
install.packages("dplyr")

library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
setwd("...")

accepts <- read.csv(file = "accepts.csv", header = TRUE)
colnames(accepts)

# Understand Target Variable #
table(accepts$bad)
table(accepts$bureau_score)
table(accepts$purpose)

accepts$good <- abs(accepts$bad - 1)
table(accepts$good)

# Setting Categorical Variables as Factors #
accepts$bankruptcy <- as.factor(accepts$bankruptcy)
accepts$used_ind <- as.factor(accepts$used_ind)
accepts$purpose <- as.factor(accepts$purpose)

# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.75*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

table(train$good)
table(test$good)

# Binning of Continuous Variables #
result <- smbinning(df = train, y = "good", x = "bureau_score")
result$ivtable
result$cut
result$iv

smbinning.plot(result,option="dist",sub="Bureau Score")
smbinning.plot(result,option="goodrate",sub="Bureau Score")
smbinning.plot(result,option="badrate",sub="Bureau Score")
smbinning.plot(result,option="WoE",sub="Bureau Score")

num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  result_all[[num_names[i]]] <- smbinning(df = train, y = "good", x = num_names[i])
}

result_all$bureau_score$ivtable #Able to pull all information within list by variable name #

# Binning of Factor Variables #
result <- smbinning.factor(df = train, y = "good", x = "purpose")
result$ivtable
result$cut
result$iv

# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary

