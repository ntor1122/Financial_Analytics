library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)
library(outliers)
library(tidyr)

rm(list=ls())

# Reads in the data.
acc <- read.csv('accepted_customers.csv') #this has 1500 default and 1500 non defaulters, dr labarr did this at weight 30-1. This despite population has 3% defaulters in accepted loans
rej <- read.csv('rejected_customers.csv') #rejected you dont know target default or not
str(acc)
glimpse(acc)
# Checks the weight variable in a table.
table(acc$GB)
table(acc$X_freq_) #because the data set given is 50-50 for good bad, we need a weight to correct to population defaulters
nrow(rej) #1498 entries

#Rejects table doeshave good,bad or weight frequency columns

# Corrects the weight variable.
all <- acc %>% mutate(freq = ifelse(GB == 1, 1, 30)) %>% select(-X_freq_)

# Creates the good variable for smbinning.
all <- all %>% mutate(good = ifelse(GB == 1, 0, 1)) %>% rename(bad = GB)

str(all)

# Imputes observations to missing when appropriate.
all <- all %>% mutate(PROF = ifelse(PROF == '', NA, PROF))
all <- all %>% mutate(PRODUCT = ifelse(PRODUCT == '', NA, PRODUCT))
all <- all %>% mutate(RESID = ifelse(RESID == '', NA, RESID))

rej <- rej %>% mutate(PROF = ifelse(PROF == '', NA, PROF))
rej <- rej %>% mutate(PRODUCT = ifelse(PRODUCT == '', NA, PRODUCT))
rej <- rej %>% mutate(RESID = ifelse(RESID == '', NA, RESID))

# Checks for missing values.
colSums(is.na(all))

# Eliminates any duplicates.
all <- all %>% unique()

rej <- rej %>% unique()

# There are no continuous variables that are highly correlated with the target.
# Checks for outliers and abnormal values.
qqnorm(all$CASH)
qqnorm(all$CHILDREN)
qqnorm(all$INCOME)
qqnorm(all$LOANS)
qqnorm(all$PERS_H)
qqnorm(all$TMADD)
qqnorm(all$TMJOB1)

# H0: the highest value is not an outlier
# HA: the highest value is an outlier
grubbs.test(all$CASH)
grubbs.test(all$CHILDREN)
grubbs.test(all$INCOME)
grubbs.test(all$LOANS)
grubbs.test(all$PERS_H)
grubbs.test(all$TMADD)
grubbs.test(all$TMJOB1)

# Checks for any separation issues.
table(all$BUREAU, all$good)
table(all$PRODUCT, all$good)
table(all$PROF, all$good)
table(all$TEL, all$good)
table(all$CARDS, all$good)

all <- all %>% 
  mutate(CARDS = ifelse(CARDS == 'VISA mybank' | CARDS == 'VISA Others', 'Visa', CARDS)) %>%
  mutate(CARDS = ifelse(CARDS == 'American Express', 'Other credit car', CARDS)) %>%
  mutate(BUREAU = ifelse(BUREAU > 1, '2+', BUREAU)) %>% 
  mutate(TEL = ifelse(TEL <= 1, '0-1', TEL)) %>%
  mutate(PROF = ifelse(is.na(PROF) == TRUE | PROF == 'Others', 'Missing/Other', PROF)) %>%
  mutate(PRODUCT = ifelse(PRODUCT == 'Others' | is.na(PRODUCT) == TRUE, 'Missing/Other', PRODUCT))

rej <- rej %>% 
  mutate(CARDS = ifelse(CARDS == 'VISA mybank' | CARDS == 'VISA Others' | CARDS == 'VISA Citibank', 'Visa', CARDS)) %>%
  mutate(CARDS = ifelse(CARDS == 'American Express', 'Other credit car', CARDS)) %>%
  mutate(BUREAU = ifelse(BUREAU > 1, '2+', BUREAU)) %>% 
  mutate(TEL = ifelse(TEL <= 1, '0-1', TEL)) %>%
  mutate(PROF = ifelse(is.na(PROF) == TRUE | PROF == 'Others', 'Missing/Other', PROF)) %>%
  mutate(PRODUCT = ifelse(PRODUCT == 'Others' | is.na(PRODUCT) == TRUE, 'Missing/Other', PRODUCT))

# Changes all categorical variables to factors.
all$BUREAU <- as.factor(all$BUREAU)
all$CAR <- as.factor(all$CAR)
all$CARDS <- as.factor(all$CARDS)
all$DIV <- as.factor(all$DIV)
all$EC_CARD <- as.factor(all$EC_CARD)
all$FINLOAN <- as.factor(all$FINLOAN)
all$LOCATION <- as.factor(all$LOCATION)
all$NAT <- as.factor(all$NAT)
all$PRODUCT <- as.factor(all$PRODUCT)
all$PROF <- as.factor(all$PROF)
all$REGN <- as.factor(all$REGN)
all$RESID <- as.factor(all$RESID)
all$TEL <- as.factor(all$TEL)
all$NMBLOAN <- as.factor(all$NMBLOAN)

# There is a 70/30 split on the training and testing data.
set.seed(12345)
train_id <- sample(seq_len(nrow(all)), size = floor(0.7*nrow(all)))
train <- all[train_id, ]
test <- all[-train_id, ]

# Bins the continuous variables.
age <- smbinning(train, y = 'good', x = 'AGE')
age
smbinning.plot(age, option = 'dist', sub = 'AGE')
smbinning.plot(age, option = 'WoE', sub = 'AGE')

cash <- smbinning(train, y = 'good', x = 'CASH')
smbinning.plot(cash, option = 'dist', sub = 'CASH')
smbinning.plot(cash, option = 'WoE', sub = 'CASH')

child <- smbinning(train, y = 'good', x = 'CHILDREN')
smbinning.plot(child, option = 'dist', sub = 'CHILDREN')
smbinning.plot(child, option = 'WoE', sub = 'CHILDREN')

pers <- smbinning(train, y = 'good', x = 'PERS_H')
smbinning.plot(pers, option = 'dist', sub = 'PERS_H')
smbinning.plot(pers, option = 'WoE', sub = 'PERS_H')

mad <- smbinning(train, y = 'good', x = 'TMADD')
smbinning.plot(mad, option = 'dist', sub = 'TMADD')
smbinning.plot(mad, option = 'WoE', sub = 'TMADD')

job <- smbinning(train, y = 'good', x = 'TMJOB1')
smbinning.plot(job, option = 'dist', sub = 'TMJOB1')
smbinning.plot(job, option = 'WoE', sub = 'TMJOB1')

# The INCOME WOE plot is jumping around - should investigate further.
inc <- smbinning(train, y = 'good', x = 'INCOME')
smbinning.plot(inc, option = 'dist', sub = 'INCOME')
smbinning.plot(inc, option = 'WoE', sub = 'INCOME')

# There are no significant splits for LOANS, so there is no statistical relationship with target.
smbinning(train, y = 'good', x = 'LOANS')

# There are less than five unique values for NMBLOAN, so this becomes a factor.
smbinning(train, y = 'good', x = 'NMBLOAN')

# Bins the factored variables.
car <- smbinning.factor(train, y = 'good', x = 'CAR')
smbinning.plot(car, option = 'dist', sub = 'CAR')
smbinning.plot(car, option = 'WoE', sub = 'CAR')
car$ivtable

div <- smbinning.factor(train, y = 'good', x = 'DIV')
smbinning.plot(div, option = 'dist', sub = 'DIV')
smbinning.plot(div, option = 'WoE', sub = 'DIV')
div$ivtable

ec <- smbinning.factor(train, y = 'good', x = 'EC_CARD')
smbinning.plot(ec, option = 'dist', sub = 'EC_CARD')
smbinning.plot(ec, option = 'WoE', sub = 'EC_CARD')
ec$ivtable

fin <- smbinning.factor(train, y = 'good', x = 'FINLOAN')
smbinning.plot(fin, option = 'dist', sub = 'FINLOAN')
smbinning.plot(fin, option = 'WoE', sub = 'FINLOAN')
fin$ivtable

loc <- smbinning.factor(train, y = 'good', x = 'LOCATION')
smbinning.plot(loc, option = 'dist', sub = 'LOCATION')
smbinning.plot(loc, option = 'WoE', sub = 'LOCATION')
loc$ivtable

nat <- smbinning.factor(train, y = 'good', x = 'NAT')
smbinning.plot(nat, option = 'dist', sub = 'NAT')
smbinning.plot(nat, option = 'WoE', sub = 'NAT')
nat$ivtable

regn <- smbinning.factor(train, y = 'good', x = 'REGN')
smbinning.plot(regn, option = 'dist', sub = 'REGN')
smbinning.plot(regn, option = 'WoE', sub = 'REGN')
regn$ivtable

resid <- smbinning.factor(train, y = 'good', x = 'RESID')
smbinning.plot(resid, option = 'dist', sub = 'RESID')
smbinning.plot(resid, option = 'WoE', sub = 'RESID')
resid$ivtable

nmb <- smbinning.factor(train, y = 'good', x = 'NMBLOAN')
smbinning.plot(nmb, option = 'dist', sub = 'NMBLOAN')
smbinning.plot(nmb, option = 'WoE', sub = 'NMBLOAN')
nmb$ivtable

# BUREAU, CARDS, PRODUCT, PROF, AND TEL have separation issues.
bureau <- smbinning.factor(train, y = 'good', x = 'BUREAU')
smbinning.plot(bureau, option = 'dist', sub = 'BUREAU')
smbinning.plot(bureau, option = 'WoE', sub = 'BUREAU')
bureau$ivtable

cards <- smbinning.factor(train, y = 'good', x = 'CARDS')
smbinning.plot(cards, option = 'dist', sub = 'CARDS')
smbinning.plot(cards, option = 'WoE', sub = 'CARDS')
cards$ivtable

prod <- smbinning.factor(train, y = 'good', x = 'PRODUCT')
smbinning.plot(prod, option = 'dist', sub = 'PRODUCT')
smbinning.plot(prod, option = 'WoE', sub = 'PRODUCT')
prod$ivtable

prof <- smbinning.factor(train, y = 'good', x = 'PROF')
smbinning.plot(prof, option = 'dist', sub = 'PROF')
smbinning.plot(prof, option = 'WoE', sub = 'PROF')
prof$ivtable

tel <- smbinning.factor(train, y = 'good', x = 'TEL')
smbinning.plot(tel, option = 'dist', sub = 'TEL')
smbinning.plot(tel, option = 'WoE', sub = 'TEL')
tel$ivtable

table(acc$LOANS) #no. of loans outside bank. NMB loan is # loans with the bank

# Calculates the information value for each variable and returns the plot.
# The strong and weak predictor variables are AGE, TMJOB1, INCOME, PERS_H, CARDS, and EC_CARDS.
# There are no significant splits in the LOAN variable.
iv_summary <- smbinning.sumiv(df = train, y = "good")
smbinning.sumiv.plot(iv_summary)
iv_summary
class(iv_summary$Char)

colnames(train)

# Bins the continuous variables in the training data to be used in the model.
multiple.func <- function(x) {
  c(is.numeric(x) | is.factor(x))
  }
num_names <- names(train)[sapply(train, multiple.func)] #only get column names from df where multiple func applied which is get only numeric and factor columns
num_names
?prop.test

prop.test(700, 1000)

check_res <- smbinning.factor(df = train, y = "good", x = num_names[8])
check_res
# $ivtable
# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate   Odds  LnOdds     WoE     IV
# 1    = '0'   1116     529    587      1116        529       587 0.5314   0.4740  0.5260 0.9012 -0.1040 -0.1059 0.0060
# 2    = '1'    984     522    462      2100       1051      1049 0.4686   0.5305  0.4695 1.1299  0.1221  0.1202 0.0068
# 3  Missing      0       0      0      2100       1051      1049 0.0000      NaN     NaN    NaN     NaN     NaN    NaN
# 4    Total   2100    1051   1049        NA         NA        NA 1.0000   0.5005  0.4995 1.0019  0.0019  0.0000 0.0128
# 
# $iv
# [1] 0.0128
# 
# $x
# [1] "FINLOAN"
# 
# $col_id
# [1] 8
# 
# $cuts
# [1] "0" "1"

result_all_sig <- list() #initialize a list
for(i in 1:length(num_names)){
  if (is.numeric(train[[num_names[i]]]) == TRUE) { #if this is numeric then do below
    check_res <- smbinning(df = train, y = "good", x = num_names[i])
    }
  if (is.factor(train[[num_names[i]]]) == TRUE){
    check_res <- smbinning.factor(df = train, y = "good", x = num_names[i])
    }
  if (check_res[1] == "Uniques values < 5"){ #these are outputs if check_res ie outputs of smbinning
    next
    }
  else if (check_res[1] == "No significant splits") {
    next
    }
  else if (check_res[1] == "Characteristic (x) not found or it is not a number"){
    next
    }
  else if (check_res$iv < 0.1){ #$iv accesses Info value of that variable 
    next
    }
  else{
    result_all_sig[[num_names[i]]] <- check_res #appending check_res result to a new list
  }
  }

result_all_sig #for all signnificant variables 6 of them list contains results of smbinning funciton output - IV, col id, cuts, woe, $x is name of variable etc etc 

# Generates new variables from the bins and WOE values for the continuous variables in the training data.
#Loop 1 for significant numeric only, generates bins
for(i in 1:length(result_all_sig)){
  if (is.numeric(train[[result_all_sig[[i]][["x"]]]]) == TRUE){ #x is name of variable
    train <- smbinning.gen(df = train, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))}}

train[["CARDS_WOE"]]
train[[bin_name]][0]
result_all_sig[["EC_CARD"]][23] #in that ec card column  or element go 23rd row

#Loop 2 for significant NUMERICS ONLY, generates WOE values
for (j in 1:length(result_all_sig)){ #entered inside list elements. per list element for every row obs
  for (i in 1:nrow(train)){
    if (is.numeric(train[[result_all_sig[[j]][["x"]]]]) == TRUE){ #oinly if it is numeric do below
      bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
      bin <- substr(train[[bin_name]][i], 2, 2)
      woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
      
      if(bin == 0){
         bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1   #dim gives you 7 lenght of list
         train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]} #here is where you create new column woename
      else{
         train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]} #this is how you grab woe values and insert into your df
    }
  }}

train 

result_all_sig
# STEPS - 1) got output from funciton, stored it in list, then accessed within list ivtable within which there is WOE and IV simple as that!

# $EC_CARD
# $EC_CARD$ivtable
# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate   Odds  LnOdds     WoE     IV
# 1    = '0'   1552     702    850      1552        702       850  0.739   0.4523  0.5477 0.8259 -0.1913 -0.1932 0.0275
# 2    = '1'    548     349    199      2100       1051      1049  0.261   0.6369  0.3631 1.7538  0.5618  0.5599 0.0797
# 3  Missing      0       0      0      2100       1051      1049  0.000      NaN     NaN    NaN     NaN     NaN    NaN
# 4    Total   2100    1051   1049        NA         NA        NA  1.000   0.5005  0.4995 1.0019  0.0019  0.0000 0.1072

# Creates WOE variables from the significant FACTORS in the training data.
train$EC_CARD_WOE <- NA
for (i in 1:nrow(train)){ #entereed inside train rows
  for (j in 1:(nrow(result_all_sig[['EC_CARD']]$ivtable)-2)){ #-2 cuz output has missing and total as last 2
    if (train$EC_CARD[i] == as.numeric(result_all_sig[['EC_CARD']]$cuts[j])){
      train$EC_CARD_WOE[i] = result_all_sig[['EC_CARD']]$ivtable[j, "WoE"]}
    }}
train

# $CARDS
# $CARDS$ivtable
# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate   Odds  LnOdds     WoE     IV
# 1      = 'Cheque card'    548     349    199       548        349       199 0.2610   0.6369  0.3631 1.7538  0.5618  0.5599 0.0797
# 2 = 'Mastercard/Euroc'     51      35     16       599        384       215 0.0243   0.6863  0.3137 2.1875  0.7828  0.7809 0.0141
# 3  = 'no credit cards'   1491     662    829      2090       1046      1044 0.7100   0.4440  0.5560 0.7986 -0.2250 -0.2269 0.0364
# 4 = 'Other credit car'      7       4      3      2097       1050      1047 0.0033   0.5714  0.4286 1.3333  0.2877  0.2858 0.0003
# 5             = 'Visa'      3       1      2      2100       1051      1049 0.0014   0.3333  0.6667 0.5000 -0.6931 -0.6951 0.0007
# 6              Missing      0       0      0      2100       1051      1049 0.0000      NaN     NaN    NaN     NaN     NaN    NaN
# 7                Total   2100    1051   1049        NA         NA        NA 1.0000   0.5005  0.4995 1.0019  0.0019  0.0000 0.1312

train$CARDS_WOE <- NA
for (i in 1:nrow(train)){
  for (j in 1:(nrow(result_all_sig[['CARDS']]$ivtable)-2)){
    if (train$CARDS[i][1] %in% result_all_sig[['CARDS']]$cuts[j]){
      train$CARDS_WOE[i] = result_all_sig[['CARDS']]$ivtable[j, "WoE"]}}}

# Builds the initial model using variables with information values greater than 0.1.
initial_score <- glm(data = train, bad ~ AGE_WOE + TMJOB1_WOE + INCOME_WOE + PERS_H_WOE + 
                       CARDS_WOE + EC_CARD_WOE, weights = train$freq, family = "binomial")
summary(initial_score)

# Evaluates the initial model on the training data.
train$pred <- initial_score$fitted.values
train

# AUC: 69.94%
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

# Generates new variables from the bins and WOE values for the continuous variables in the testing data.
for(i in 1:length(result_all_sig))
{if (is.numeric(test[[result_all_sig[[i]][["x"]]]]) == TRUE)
{test <- smbinning.gen(df = test, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))}}

for (j in 1:length(result_all_sig))
{for (i in 1:nrow(test))
{if (is.numeric(test[[result_all_sig[[j]][["x"]]]]) == TRUE)
{bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
bin <- substr(train[[bin_name]][i], 2, 2)
woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
if(bin == 0)
{bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}
else 
{test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}}}}

# Creates WOE variables from the significant factors in the testing data.
test$EC_CARD_WOE <- NA
for (i in 1:nrow(test))
{for (j in 1:(nrow(result_all_sig[['EC_CARD']]$ivtable)-2))
{if (test$EC_CARD[i] == as.numeric(result_all_sig[['EC_CARD']]$cuts[j]))
{test$EC_CARD_WOE[i] = result_all_sig[['EC_CARD']]$ivtable[j, "WoE"]}}}

test$CARDS_WOE <- NA
for (i in 1:nrow(test))
{for (j in 1:(nrow(result_all_sig[['CARDS']]$ivtable)-2))
{if (test$CARDS[i][1] %in% result_all_sig[['CARDS']]$cuts[j])
{test$CARDS_WOE[i] = result_all_sig[['CARDS']]$ivtable[j, "WoE"]}}}

# Evaluates the initial model on the testing data.
test$pred <- predict(initial_score, newdata = test, type='response')

# AUC: 53.78%
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

###################################NO NEED TO DO BELOW WITHOUT REJECT INFERENCING#######################
# Adds scores to the initial model.
# The Bank assigns a score of 500 to applicants with odds-ratio 20:1.
# Doubling the odds is associated with a change of 50 points in the scorecard.
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) 
{beta <- initial_score$coefficients[i]
beta0 <- initial_score$coefficients["(Intercept)"]
nvar <- length(var_names)
WOE_var <- train[[i]]
points_name <- paste(str_sub(i, end = -4), "points", sep = "")
train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar}

colini <- (ncol(train) - nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, main = "Distribution of Train Scores", xlab = "Score")

for(i in var_names)
{beta <- initial_score$coefficients[i]
beta0 <- initial_score$coefficients["(Intercept)"]
nvar <- length(var_names)
WOE_var <- test[[i]]
points_name <- paste(str_sub(i, end = -4), "points", sep = "")
test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar}

colini <- (ncol(test) - nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

hist(test$Score, breaks = 50, main = "Distribution of Test Scores", xlab = "Score")

# Combined the training and testing data.
accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

# Plots default by score in the training data.
# Creation of a distribution to associate score buckets with default rate.
cutpoints <- quantile(train$Score, probs = seq(0,1,0.10))
train$Score.QBin <- cut(train$Score, breaks = cutpoints, include.lowest = TRUE)
Default.QBin.train <- round(table(train$Score.QBin, train$bad)[,2]/rowSums(table(train$Score.QBin, train$bad))*100,2)
print(Default.QBin.train)

Default.QBin.train.pop <- round(table(train$Score.QBin, train$bad)[,2]/(table(train$Score.QBin, train$bad)[,2] + table(train$Score.QBin, train$bad)[,1]*30)*100,2)
print(Default.QBin.train.pop)

# Plots default by score in the testing data.
cutpoints <- quantile(test$Score, probs = seq(0,1,0.10))
test$Score.QBin <- cut(test$Score, breaks = cutpoints, include.lowest = TRUE)
Default.QBin.test <- round(table(test$Score.QBin, test$bad)[,2]/rowSums(table(test$Score.QBin, test$bad))*100,2)

print(Default.QBin.test)

barplot(Default.QBin.test, main = "Default Decile Plot", xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,60), col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 50, lwd = 2, lty = "dashed")
text(11, 52, "Current = 50%")

Default.QBin.test.pop <- round(table(test$Score.QBin, test$bad)[,2]/(table(test$Score.QBin, test$bad)[,2] + table(test$Score.QBin, test$bad)[,1]*30)*100,2)
print(Default.QBin.test.pop)

barplot(Default.QBin.test.pop, main = "Default Decile Plot", xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,5), col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 3.23, lwd = 2, lty = "dashed")
text(11, 4, "Current = 3.23%")

# Plots the default, acceptance, and profit by score.
def <- NULL
acc <- NULL
prof <- NULL
score <- NULL

cost <- 52000
profit <- 2000
for(i in min(floor(train$Score)):max(floor(train$Score)))
{score[i - min(floor(train$Score)) + 1] <- i
def[i - min(floor(train$Score)) + 1] <- 100*sum(train$bad[which(train$Score >= i)])/(length(train$bad[which(train$Score >= i & train$bad == 1)]) + 30*length(train$bad[which(train$Score >= i & train$bad == 0)]))
acc[i - min(floor(train$Score)) + 1] <- 100*(length(train$bad[which(train$Score >= i & train$bad == 1)]) + 30*length(train$bad[which(train$Score >= i & train$bad == 0)]))/(length(train$bad[which(train$bad == 1)]) + 30*length(train$bad[which(train$bad == 0)]))
prof[i - min(floor(train$Score)) + 1] <- length(train$bad[which(train$Score >= i & train$bad == 1)])*(-cost) + 30*length(train$bad[which(train$Score >= i & train$bad == 0)])*profit}
plot_data <- data.frame(def, acc, prof, score)

def_plot <- xyplot(def ~ score, plot_data, type = "l", lwd = 2, col = "red",
                   ylab = "Default Rate (%)", xlab = "Score",
                   main = "Default Rate by Acceptance Across Score",
                   panel = function(x, y,...) {panel.xyplot(x, y, ...)
                     panel.abline(h = 5.00, col = "red")})

acc_plot <- xyplot(acc ~ score, plot_data, type = "l", lwd=2, col="blue",
                   ylab = "Acceptance Rate (%)", 
                   panel = function(x, y,...) {panel.xyplot(x, y, ...)
                     panel.abline(h = 70, col = "blue")})

prof_plot <- xyplot(prof/1000000 ~ score, plot_data, type = "l", lwd = 2, col = "green",
                    ylab = "Profit (Millions $)", xlab = "Score",
                    main = "Profit by Acceptance Across Score")

doubleYScale(def_plot, acc_plot, add.ylab2 = TRUE, use.style = FALSE)
doubleYScale(prof_plot, acc_plot, add.ylab2 = TRUE, use.style = FALSE)

ay1 <- list(title = "Default Rate (%)", range = c(0, 6))
ay2 <- list(tickfont = list(), range = c(0, 100), overlaying = "y", side = "right",
            title = "Acceptance Rate (%)")
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~def, name = "Default Rate (%)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(title = "Default Rate by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
                      xaxis = list(title = "Scorecard Value"), legend = list(x = 1.2, y = 0.8))
fig

ay1 <- list(title = "Profit ($)", showline = FALSE, showgrid = FALSE)
ay2 <- list(tickfont = list(), range = c(0, 100), overlaying = "y", side = "right",
            title = "Acceptance Rate (%)")
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~prof, name = "Profit ($)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(title = "Profit by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
                      xaxis = list(title = "Scorecard Value"), legend = list(x = 1.2, y = 0.8))
fig

# #####################################Skip to this. Reject Inference################################################################################
# The rejected data set maintains the existing acceptance rate of 75%.
# Randomly chose 1,125 observations from the rejected applicants.
set.seed(12345)
rej <- sample_n(rej, 1125) # before rejected was 1498 obs, Sample down to 1125 cuz population has 75% accept, 25% reject. We already have 3000 obs in accepted data set. 

result_all_sig
# Bins the continuous variables in the rejected data to be used in the model.
for(i in names(result_all_sig)){
  print(i)
  result_all_sig[[i]]$bands[1] <- min(c(all[[i]], rej[[i]]), na.rm = TRUE)
  result_all_sig[[i]]$bands[length(result_all_sig[[i]]$bands)] <- max(c(all[[i]], rej[[i]]), na.rm = TRUE)}

# Generates new variables from the bins and WOE values for the continuous variables in the rejected data.
rejects_scored <- rej
for(i in 1:length(result_all_sig)){
  if (is.numeric(test[[result_all_sig[[i]][["x"]]]]) == TRUE){
    rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))}}

for (j in 1:length(result_all_sig))
{for (i in 1:nrow(rejects_scored))
{if (is.numeric(test[[result_all_sig[[j]][["x"]]]]) == TRUE)
{bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
bin <- substr(rejects_scored[[bin_name]][i], 2, 2)
woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
if(bin == 0)
{bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}
else 
{rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}}}}

# Creates WOE variables from the significant factors in the rejected data.
rejects_scored$EC_CARD_WOE <- NA
for (i in 1:nrow(rejects_scored))
{for (j in 1:(nrow(result_all_sig[['EC_CARD']]$ivtable)-2))
{if (rejects_scored$EC_CARD[i] == as.numeric(result_all_sig[['EC_CARD']]$cuts[j]))
{rejects_scored$EC_CARD_WOE[i] = result_all_sig[['EC_CARD']]$ivtable[j, "WoE"]}}}

rejects_scored$CARDS_WOE <- NA
for (i in 1:nrow(rejects_scored))
{for (j in 1:(nrow(result_all_sig[['CARDS']]$ivtable)-2))
{if (rejects_scored$CARDS[i][1] %in% result_all_sig[['CARDS']]$cuts[j])
{rejects_scored$CARDS_WOE[i] = result_all_sig[['CARDS']]$ivtable[j, "WoE"]}}}

# Adds scores to the initial model.
# The Bank assigns a score of 500 to applicants with odds-ratio 20:1.
# Doubling the odds is associated with a change of 50 points in the scorecard.
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) 
{beta <- initial_score$coefficients[i]
beta0 <- initial_score$coefficients["(Intercept)"]
nvar <- length(var_names)
WOE_var <- rejects_scored[[i]]
points_name <- paste(str_sub(i, end = -4), "points", sep="")
rejects_scored[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar}

colini <- (ncol(rejects_scored) - nvar + 1)
colend <- ncol(rejects_scored)
rejects_scored$Score <- rowSums(rejects_scored[, colini:colend])

# Reject Inference (Hard Cut-Off)
# Used the KS Statistic (Youden's Index) of 0.3066 for the optimal cut-off (0.0321).
rejects_scored$pred <- predict(initial_score, newdata = rejects_scored, type = 'response')
rej$bad <- as.numeric(rejects_scored$pred > 0.0321) #created new column bad based on this curoff from KS stat
rej$good <- abs(rej$bad - 1)
all <- all %>% select(-freq)

# Used a total population of 30,000 for the calculations where 3.23% default.
pop_g <- 29031
pop_b <- 969

# The sample has 1,500 good applicants and 1,500 bad applicants.
sam_g <- 1500
sam_b <- 1500

# The Population/Sample to Good/Bad Ratio is 29.95975. THis is default ratio. 
pop_sam_gb_ratio <- (pop_g/pop_b)/(sam_g/sam_b)

# The rejected applicants make up 25% of the total population.
pop_a <- 0.75
pop_r <- 0.25

# The sample has 3,000 accepted and 1,500 rejected applicants.
sam_a <- 3000
sam_r <- 1500

# The Population/Sample to Accepted/Rejected Ratio is 1.5. This is accepted rejected ratio. 
pop_sam_ar_ratio <- (pop_a/pop_r)/(sam_a/sam_r)

weight_rb <- 1
weight_rg <- pop_sam_gb_ratio #your ratio for good bad default 3% in population
weight_ab <- pop_sam_ar_ratio #your ratio for accepted rejected (75-25 population)

weight_ag <- pop_sam_ar_ratio*pop_sam_gb_ratio #Accepted reject ratio * Good Bad Default Ratio

all$weight_ar <- ifelse(all$bad == 1, weight_ab, weight_ag) #all is same as accepted. train was got from all. 
rej$weight_ar <- ifelse(rej$bad == 1, weight_rb, weight_rg)

all
rej

# Changes all factored variables to characters.
all$BUREAU <- as.character(all$BUREAU)
all$CAR <- as.character(all$CAR)
all$CARDS <- as.character(all$CARDS)
all$DIV <- as.character(all$DIV)
all$EC_CARD <- as.character(all$EC_CARD)
all$FINLOAN <- as.character(all$FINLOAN)
all$LOCATION <- as.character(all$LOCATION)
all$NAT <- as.character(all$NAT)
all$PRODUCT <- as.character(all$PRODUCT)
all$PROF <- as.character(all$PROF)
all$REGN <- as.character(all$REGN)
all$RESID <- as.character(all$RESID)
all$TEL <- as.character(all$TEL)
all$NMBLOAN <- as.character(all$NMBLOAN)

# Combines the accepted and rejected data sets.
# The PROF and PRODUCT variables include commas and returns a missing IV, so these are removed.
all
comb_hard <- rbind(all[, !(names(all) == 'weight')], rej)  #all is df. not equal to columna name weight. here is wheree you combined reject dataset
comb_hard

comb_hard$PROF <- str_replace_all(comb_hard$PROF, ',', '')
comb_hard$PRODUCT <- str_replace_all(comb_hard$PRODUCT, ',', '')

# The IV returns that PROF has too many categories, so they are combined. ifelse(xyz='asdasd', do this, ifelse())
comb_hard <- comb_hard %>% mutate(RESID = ifelse(is.na(RESID) == TRUE, 'Missing', RESID)) %>%
                           mutate(PROF = ifelse(PROF == 'Civil Service M', 'Civil Service', 
                                                ifelse(PROF == 'FoodBuildingCa', 'Food or Building',
                                                       ifelse(PROF == 'Sea Vojage Gast', 'Sea Vojage',
                                                               ifelse(PROF == 'StateSteel Ind', 'State or Steel Ind', PROF))))) #else leave prof as prof

comb_hard <- comb_hard %>% mutate(PRODUCT = ifelse(PRODUCT == 'Dept. StoreMail', 'Dept. Store or Mail',
                                                   ifelse(PRODUCT == 'FurnitureCarpet', 'Furniture or Carpet',
                                                          ifelse(PRODUCT == 'Radio TV Hifi', 'Radio or TV or Hifi', PRODUCT))))

# Changes all the categorical variables back to factors.
comb_hard$BUREAU <- as.factor(comb_hard$BUREAU)
comb_hard$CAR <- as.factor(comb_hard$CAR)
comb_hard$CARDS <- as.factor(comb_hard$CARDS)
comb_hard$DIV <- as.factor(comb_hard$DIV)
comb_hard$EC_CARD <- as.factor(comb_hard$EC_CARD)
comb_hard$FINLOAN <- as.factor(comb_hard$FINLOAN)
comb_hard$LOCATION <- as.factor(comb_hard$LOCATION)
comb_hard$NAT <- as.factor(comb_hard$NAT)
comb_hard$PRODUCT <- as.factor(comb_hard$PRODUCT)
comb_hard$PROF <- as.factor(comb_hard$PROF)
comb_hard$REGN <- as.factor(comb_hard$REGN)
comb_hard$RESID <- as.factor(comb_hard$RESID)
comb_hard$TEL <- as.factor(comb_hard$TEL)
comb_hard$NMBLOAN <- as.factor(comb_hard$NMBLOAN)

# Builds the final scorecard model.
comb <- comb_hard

# There is a 70/30 split on the training and testing data.
set.seed(12345)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.7*nrow(comb)))
train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

# Calculates the information value for each variable and returns the plot.
# The strong/weak predictors are AGE, TMJOB1, CARDS, EC_CARDS, and PERS_H, and CHILDREN.
# INCOME, LOAN, and CASH have no significant splits.
iv_summary <- smbinning.sumiv(df = train_comb, y = "good")
smbinning.sumiv.plot(iv_summary)
iv_summary

# Bins the continuous variables in the combined training data to be used in the final model.
multiple.func <- function(x) {c(is.numeric(x) | is.factor(x))}
num_names <- names(train_comb)[sapply(train_comb, multiple.func)]
result_all_sig <- list()
for(i in 1:length(num_names))
{if (is.numeric(train_comb[[num_names[i]]]) == TRUE)
{check_res <- smbinning(df = train_comb, y = "good", x = num_names[i])}
if (is.factor(train_comb[[num_names[i]]]) == TRUE)
{check_res <- smbinning.factor(df = train_comb, y = "good", x = num_names[i])}
if (check_res[1] == "Uniques values < 5")
{next}
else if (check_res[1] == "No significant splits")
{next}
else if (check_res[1] == "Characteristic (x) not found or it is not a number")
{next}
else if (check_res$iv < 0.1) #this was not running earlier. reason is you need to collapse to fewer categories up above where you used str replace.
{next}
else
{result_all_sig[[num_names[i]]] <- check_res}}

# Generates new variables from the bins and WOE values for the continuous variables in the combined training data.
for(i in 1:length(result_all_sig))
{if (is.numeric(train_comb[[result_all_sig[[i]][["x"]]]]) == TRUE)
{train_comb <- smbinning.gen(df = train_comb, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))}}

for (j in 1:length(result_all_sig))
{for (i in 1:nrow(train_comb))
{if (is.numeric(train_comb[[result_all_sig[[j]][["x"]]]]) == TRUE)
{bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
bin <- substr(train_comb[[bin_name]][i], 2, 2)
woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
if(bin == 0)
{bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
train_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}
else 
{train_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}}}}

# Creates WOE variables from the significant factors in the combined training data.
train_comb$EC_CARD_WOE <- NA
for (i in 1:nrow(train_comb))
{for (j in 1:(nrow(result_all_sig[['EC_CARD']]$ivtable)-2))
{if (train_comb$EC_CARD[i] == as.numeric(result_all_sig[['EC_CARD']]$cuts[j]))
{train_comb$EC_CARD_WOE[i] = result_all_sig[['EC_CARD']]$ivtable[j, "WoE"]}}}

train_comb$CARDS_WOE <- NA
for (i in 1:nrow(train_comb))
{for (j in 1:(nrow(result_all_sig[['CARDS']]$ivtable)-2))
{if (train_comb$CARDS[i][1] %in% result_all_sig[['CARDS']]$cuts[j])
{train_comb$CARDS_WOE[i] = result_all_sig[['CARDS']]$ivtable[j, "WoE"]}}}

# Builds the final model using variables with information values greater than 0.1.
# There is are new variables, TEL and PRODUCT, that are now significant variables in the model.
# The INCOME variable is no longer included.
final_score <- glm(data = train_comb, bad ~ AGE_WOE + TMJOB1_WOE + PERS_H_WOE + CARDS_WOE + 
                     EC_CARD_WOE + CHILDREN_WOE, weights = train_comb$weight_ar, 
                   family = "binomial")
summary(final_score)

# Evaluates the final model on the combined training data.
train_comb$pred <- final_score$fitted.values

# AUC: 78.9%
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

# Generates new variables from the bins and WOE values for the continuous variables in the combined testing data.
for(i in 1:length(result_all_sig))
{if (is.numeric(test_comb[[result_all_sig[[i]][["x"]]]]) == TRUE)
{test_comb <- smbinning.gen(df = test_comb, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))}}

for (j in 1:length(result_all_sig))
{for (i in 1:nrow(test_comb))
{if (is.numeric(test_comb[[result_all_sig[[j]][["x"]]]]) == TRUE)
{bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
bin <- substr(test_comb[[bin_name]][i], 2, 2)
woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
if(bin == 0)
{bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
test_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}
else 
{test_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]}}}}

# Creates WOE variables from the significant factors in the combined testing data.
test_comb$EC_CARD_WOE <- NA
for (i in 1:nrow(test_comb))
{for (j in 1:(nrow(result_all_sig[['EC_CARD']]$ivtable)-2))
{if (test_comb$EC_CARD[i] == as.numeric(result_all_sig[['EC_CARD']]$cuts[j]))
{test_comb$EC_CARD_WOE[i] = result_all_sig[['EC_CARD']]$ivtable[j, "WoE"]}}}

test_comb$CARDS_WOE <- NA
for (i in 1:nrow(test_comb))
{for (j in 1:(nrow(result_all_sig[['CARDS']]$ivtable)-2))
{if (test_comb$CARDS[i][1] %in% result_all_sig[['CARDS']]$cuts[j])
{test_comb$CARDS_WOE[i] = result_all_sig[['CARDS']]$ivtable[j, "WoE"]}}}

# Evaluates the final model on the combined testing data.
test_comb$pred <- predict(final_score, newdata = test_comb, type = 'response')

# AUC: 78.61%
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

# Adds scores to the final model.
# The Bank assigns a score of 500 to applicants with odds-ratio 20:1.
# Doubling the odds is associated with a change of 50 points in the scorecard.
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(final_score$coefficients[-1])


#IMp for loop to assign points per each bin per variable
for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep = "")
  train_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
  } #create column name, below is calc for that column

  colini <- (ncol(train_comb)-nvar + 1)
  colend <- ncol(train_comb)
  train_comb$Score <- rowSums(train_comb[, colini:colend])

hist(train_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")


#IMp for loop to assign points per each bin per variable
for(i in var_names[1:2])
{beta <- final_score$coefficients[i]
print(beta)
beta0 <- final_score$coefficients["(Intercept)"]
print(beta0)
nvar <- length(var_names)
WOE_var <- test_comb[[i]]
print(WOE_var)
points_name <- paste(str_sub(i, end = -4), "points", sep = "") #create column name, below is calc for that column
print(points_name)
test_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar} #this is where you assigning points based on linear eqn for each bin range for each variable
print(test_comb[[points_name]])



colini <- (ncol(test_comb)-nvar + 1)
colend <- ncol(test_comb)
test_comb$Score <- rowSums(test_comb[, colini:colend]) #before above you got broke down of total score by each variable bin, now row sum to get total point for that obs

hist(test_comb$Score, breaks = 50, main = "Distribution of Test Scores", xlab = "Score")

# Combines the training and testing data.
accepts_scored_comb <- rbind(train_comb, test_comb)
hist(accepts_scored_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

# Plots default by score in the combined data.
# Creation of a distribution to associate score buckets with default rate.
cutpoints <- quantile(accepts_scored_comb$Score, probs = seq(0,1,0.10))
accepts_scored_comb$Score.QBin <- cut(accepts_scored_comb$Score, breaks = cutpoints, include.lowest=TRUE)
Default.QBin.pop <- round(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[,2]/(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[,2] + table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[,1]*30)*100,2)
print(Default.QBin.pop)

barplot(Default.QBin.pop, main = "Default Decile Plot", xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,30), col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 3.23, lwd = 2, lty = "dashed")
text(11, 5, "Current = 3.23%")

# Plots the default, acceptance, and profit by score.
def <- NULL
acc <- NULL
prof <- NULL
score <- NULL
cost <- 50000
profit <- 1200

#for each score do this X axxis, calc profit and acc rate  and default 
for(i in min(floor(train_comb$Score)):max(floor(train_comb$Score))) #for each score do this X axxis, calc profit and acc rate  and default rate
{score[i - min(floor(train_comb$Score)) + 1] <- i
def[i - min(floor(train_comb$Score)) + 1] <- 100*sum(train_comb$bad[which(train_comb$Score >= i)])/(length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 1)]) + 30*length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 0)]))
acc[i - min(floor(train_comb$Score)) + 1] <- 100*(length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 1)]) + 30*length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 0)]))/(length(train_comb$bad[which(train_comb$bad == 1)]) + 30*length(train_comb$bad[which(train_comb$bad == 0)]))
prof[i - min(floor(train_comb$Score)) + 1] <- length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 1)])*(-cost) + 30*length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 0)])*profit}
plot_data <- data.frame(def, acc, prof, score)

def_plot <- xyplot(def ~ score, plot_data, type = "l", lwd = 2, col = "red",
                   ylab = "Default Rate (%)", xlab = "Score",
                   main = "Default Rate by Acceptance Across Score",
                   panel = function(x, y,...) {panel.xyplot(x, y, ...)
                     panel.abline(h = 5.00, col = "red")})

acc_plot <- xyplot(acc ~ score, plot_data, type = "l", lwd = 2, col = "blue",
                   ylab = "Acceptance Rate (%)", 
                   panel = function(x, y,...) {panel.xyplot(x, y, ...)
                     panel.abline(h = 70, col = "blue")})

prof_plot <- xyplot(prof/1000000 ~ score, plot_data, type = "l", lwd = 2, col = "green",
                    ylab = "Profit (Millions $)",xlab = "Score",
                    main = "Profit by Acceptance Across Score")

doubleYScale(def_plot, acc_plot, add.ylab2 = TRUE, use.style = FALSE)
doubleYScale(prof_plot, acc_plot, add.ylab2 = TRUE, use.style = FALSE)

ay1 <- list(title = "Default Rate (%)", range = c(0, 10))
ay2 <- list(tickfont = list(), range = c(0, 100), overlaying = "y", side = "right",
            title = "Acceptance Rate (%)")
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~def, name = "Default Rate (%)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(title = "Default Rate by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
                      xaxis = list(title = "Scorecard Value"), legend = list(x = 1.2, y = 0.8))
fig

ay1 <- list(title = "Profit ($)", showline = FALSE, showgrid = FALSE)
ay2 <- list(tickfont = list(),range = c(0, 100), overlaying = "y", side = "right",
            title = "Acceptance Rate (%)")
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~prof, name = "Profit ($)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(title = "Profit by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
                      xaxis = list(title = "Scorecard Value"), legend = list(x = 1.2, y = 0.8))
fig

# Creates and prints the final scorecard from the data.
final_scorecard <- accepts_scored_comb %>% 
  select(PERS_H_bin, CARDS, AGE_bin, TMJOB1_bin, EC_CARD, AGE_points, TMJOB1_points, CHILDREN_bin,
         PERS_H_points, CARDS_points, EC_CARD_points, CHILDREN_points)
final_scorecard 
#Go wide to long here
final_scorecard <- final_scorecard %>% 
  pivot_longer(cols = c('AGE_bin', 'TMJOB1_bin', 'PERS_H_bin', 'CARDS', 'EC_CARD', 'CHILDREN_bin'),
               names_to = 'Variable', values_to = 'Level')

# Go wide to long again
final_scorecard <- final_scorecard %>% 
  pivot_longer(cols = c('AGE_points', 'TMJOB1_points', 'PERS_H_points', 'CARDS_points', 
                        'EC_CARD_points', 'CHILDREN_points'),
               names_to = 'Variable_Points', values_to = 'Points')

final_scorecard$Variable <- str_replace_all(final_scorecard$Variable, '_bin', '')
final_scorecard$Variable_Points <- str_replace_all(final_scorecard$Variable_Points, '_points', '')

final_scorecard <- final_scorecard %>% filter(Variable == Variable_Points) %>% 
  select(-Variable_Points) %>% arrange(Variable, Level) %>% unique()

final_scorecard$Points <- as.integer(final_scorecard$Points)
