require("readxl") 

#Read data
df= read_xlsx("C:/Users/Bhavya Tandon/Documents/Analytics R/Assignments R/R - Linear Regression case study/Linear Regression Case.xlsx")

names(df)

# Y Variable : Total spend of credit card(Primary Card + Secondary card)
df['TotalAmt'] <- df$cardspent + df$card2spent
df["cardspent"] <- NULL
df["card2spent"] <- NULL


#Dropping log value columns of existing columns.
droplogVar <- c("lncardmon","lncardten",'lncreddebt',"lnequipmon",'lnequipten',"lninc","lnlongmon",'lnlongten',"lnothdebt","lntollmon","lntollten","lnwiremon","lnwireten")
df[droplogVar] <- NULL

# Dropping Custid,bithmonth,callerID: Does not have any property or effect on Y
df["custid"] <- NULL
df["birthmonth"] <- NULL
df["callid"] <- NULL

View(data.frame(sapply(df,class)))

##################### Data Prep Level 1 ############################


### UDF to analyse the numeric variables
mystats <- function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
}


analyze.data <- data.frame(sapply(df,mystats))
write.csv(analyze.data,file="analyzedDatastats.csv")


# Outlier detection

outlier_treatment <- function(x){
  UC = quantile(x, p = 0.95, na.rm = T)
  LC = quantile(x, p = 0.05, na.rm = T)
  x = ifelse(x>UC, UC, x)
  x = ifelse(x<LC, LC, x)
  return(x)
}
df_outlier_treated <- data.frame(apply(df, 2, FUN = outlier_treatment))

#Missing Value treatment
missing_value_treatment <- function(x){
  x[is.na(x)] = mean(x, na.rm = T)
  return(x)
}

df_outlier_missing_treated <- data.frame(apply(df_outlier_treated, 2, FUN = missing_value_treatment))

analysis.num.vars.treated <- data.frame(t(apply(df_outlier_missing_treated, 2, mystats)))
write.csv(analysis.num.vars.treated, file = 'TreatedValues_Analysis.csv')

##################### Data Prep Level 2: Feature Selection ############################

correlations <- data.frame(cor(df_outlier_missing_treated))
write.csv(correlations,file="correlations.csv")

# dependent and independent variable
require(dplyr)


features <- dplyr::select(df_outlier_missing_treated, -TotalAmt)
Y_var <- dplyr::select(df_outlier_missing_treated, TotalAmt)

features <- data.matrix(features)
Y_var <- data.matrix(Y_var)

# Stepwise Selection Method -

# Full Model
modelF <- lm(TotalAmt~.,data=df_outlier_missing_treated)
modelF

# Empty Model
modelnull <- lm(TotalAmt~1,data=df_outlier_missing_treated)
modelnull

# Runningg the Stepwise Regression to find the imp variables
stepwise.model <- step(modelnull, scope = list(upper = modelF),   data =df_outlier_missing_treated , direction = "both")

# Important predictors by stepwise method.

"card2items" ,"carditems" ,"inccat" ,"card" ,"card2" ,"income" ,
  "gender" ,"age" ,"churn" ,"pets_cats" ,"commutewalk" ,"region" ,
  "townsize" ,"creddebt" ,"multline" ,"ownpc" ,"cardtenure" ,"callcard" ,
  "card2fee" ,"response_03" ,"card2type" ,"pets_dogs" ,"agecat" ,
  "cardbenefit" ,"cardtenurecat" ,"owndvd" ,"ownpda" ,"voice"


# Recursive Feature Selection method: (RFE)
# Full Model
modelF <- lm(TotalAmt~.,data=df_outlier_missing_treated)
set.seed(4)

require(caret)

rfe_model <- caret::rfe(features,Y_var,size=c(1:114), rfeControl=rfeControl(functions = lmFuncs))

rfe_top10vars <- update(rfe_model,features,Y_var, size = 10)

rfe_top10vars[["bestVar"]]

# As per the RFE output,  10 most important vars are -

"card","marital","card2items", "carditems","card2","inccat","equip",   
"tollfree","voice","gender" 

Selected.Vars<- c("card2items" ,"carditems" ,"inccat" ,"card" ,"card2" ,"income" ,
                  "gender" ,"age" ,"churn" ,"pets_cats" ,"commutewalk" ,"region" ,
                  "townsize" ,"creddebt" ,"multline" ,"ownpc" ,"cardtenure" ,"callcard" ,
                  "card2fee" ,"response_03" ,"card2type" ,"pets_dogs" ,"agecat" ,
                  "cardbenefit" ,"cardtenurecat" ,"owndvd" ,"ownpda" ,"voice","card","marital","card2items", "carditems","card2","inccat","equip",   
                  "tollfree","voice","gender")
predictors <- Selected.Vars[!duplicated(Selected.Vars)]

df_predictors <- df_outlier_missing_treated[predictors]
df_predictors <- cbind(df_predictors,df_outlier_missing_treated["TotalAmt"])

##### LASSO #####

# Initalising a lasso regression model
require(glmnet)
lasso = train(TotalAmt~.,
              data=df_predictors, method='glmnet',
              trControl = trainControl(method="none"),
              tuneGrid=expand.grid(alpha=1,lambda=0.09))

coef(lasso$finalModel, s = lasso$bestTune$lambda)




##### VIF #####

require(car)
modelF <- lm(TotalAmt~., data = df_predictors)
car::vif(modelF)

# Removing agecat  and running VIF again 
# Since we have Age also as an important predictor, category to which age belongs can be find out.

df_vif_predictor<- dplyr::select(df_predictors,-"agecat")
modelF <- lm(TotalAmt~., data = df_vif_predictor)
car::vif(modelF)

# Removing inccat  and running VIF again 
# Since we have income (Category to which income belongs can be find out easily)

df_vif_predictor2<- dplyr::select(df_vif_predictor,-"inccat")
modelF <- lm(TotalAmt~., data = df_vif_predictor2)
car::vif(modelF)


######### 29 Important predictors after feature selection  method -##########
  
card2items+carditems+income + card+card2+gender+
churn + pets_cats+ commutewalk + region +townsize  +creddebt +
multline +ownpc +cardtenure +callcard + card2fee+ response_03+
card2type  +pets_dogs+  age + cardbenefit+ cardtenurecat+owndvd +ownpda 
+voice+  marital +equip +tollfree 


####### Dividing the data into train (development) and test (Validation) ######

selected_rows <- sample(1:nrow(df_vif_predictor2), floor(nrow(df_vif_predictor2)*0.7))

dev <- df_vif_predictor2[selected_rows,]

val <- df_vif_predictor2[-selected_rows,]

nrow(df_vif_predictor2)
nrow(dev)
nrow(val)

nrow(dev) / nrow(df_vif_predictor2)  # 70% in the train data (dev data)
nrow(val) / nrow(df_vif_predictor2)  # 30% in the test data (val data)

################# FITTING THE MODEL ################

model1 <- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               churn + pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc +cardtenure +callcard + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+ cardtenurecat+owndvd +ownpda 
             +voice+  marital +equip +tollfree ,
             data = dev)
summary(model1)

model2 <- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               churn + pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc +cardtenure +callcard + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+ cardtenurecat+owndvd +ownpda 
             +voice+  marital +tollfree ,
             data = dev)
summary(model2)


model3 <- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               churn + pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc +cardtenure +callcard + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+owndvd +ownpda 
             +voice+  marital + tollfree ,
             data = dev)
summary(model3)

model4 <- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               churn + pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc +cardtenure +callcard + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+owndvd +ownpda 
             +voice+  tollfree ,
             data = dev)
summary(model4)

model5 <- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc +cardtenure +callcard + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+owndvd +ownpda 
             +voice+  tollfree ,
             data = dev)
summary(model5)

model6 <- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc +callcard + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+owndvd +ownpda 
             +voice+  tollfree ,
             data = dev)
summary(model6)

model7 <- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+owndvd +ownpda 
             +voice+  tollfree ,
             data = dev)
summary(model7)


model8<- lm(TotalAmt~  
               card2items+
               carditems+ 
               income + card+card2+gender+
               pets_cats+ commutewalk + region +townsize  +creddebt +
               multline +ownpc + card2fee+ response_03+
               card2type  +pets_dogs+  age+ cardbenefit+owndvd 
             +voice+  tollfree ,
             data = dev)
summary(model8)

model9 <- lm(TotalAmt~  
              card2items+
              carditems+ 
              income + card+card2+gender+
              pets_cats+ region +townsize  +creddebt +
              multline +ownpc + card2fee+ response_03+
              card2type  +pets_dogs+  age+ cardbenefit+owndvd 
            +voice+  tollfree ,
            data = dev)
summary(model9)

model10 <- lm(TotalAmt~  
              card2items+
              carditems+ 
              income + card+card2+gender+
              pets_cats+ region +townsize+
              multline +ownpc + card2fee+ response_03+
              card2type  +pets_dogs+  age+ cardbenefit+owndvd 
            +voice+  tollfree ,
            data = dev)
summary(model10)


model11 <- lm(TotalAmt~  
                card2items+
                carditems+ 
                income + card+card2+gender+
                pets_cats+ region +townsize+
                multline +ownpc + card2fee+ response_03+
                card2type  +age+ cardbenefit+owndvd 
              +voice+  tollfree ,
              data = dev)
summary(model11)


model12 <- lm(TotalAmt~  
                card2items+
                carditems+ 
                income + card+card2+gender+
                pets_cats+ region +townsize+
                multline +ownpc + card2fee+ response_03+
                age+ cardbenefit+owndvd 
              +voice+  tollfree,
              data = dev)
summary(model12)

############### COOK'S Distance to reduce error & improve accuracy ##################

cooksDist <- cooks.distance(model12)
SampleSize <- nrow(dev)

par(mar=c(1,1,1,1))
plot(cooksDist, pch = "*", cex = 2, main = "The data points that are influential as per Cooks Distance")
abline(h = 4/SampleSize, col = "yellow")
text(x=1:length(cooksDist)+1, y=cooksDist, labels=ifelse(cooksDist>4/SampleSize, names(cooksDist),""), col="red3") 

# Removing those observations that are influencing the line of best fit

influential <- as.numeric(names(cooksDist)[(cooksDist > (4/SampleSize))])

dev.CooksDist <- dev[-influential,]

View(dev.CooksDist)


model13 <- lm(TotalAmt~ card2items+
                carditems+ 
                income + card+card2+gender+
                pets_cats+ region +townsize+
                multline +ownpc + card2fee+ response_03+
                age+ cardbenefit+owndvd 
              +voice+  tollfree,
             data = dev.CooksDist)
summary(model13)

model14 <- lm(TotalAmt~ card2items+
                carditems+ 
                income + card+card2+gender+
                region +townsize+
                multline +ownpc + card2fee+ response_03+
                age+ cardbenefit+owndvd 
              +voice+  tollfree,
              data = dev.CooksDist)
summary(model14)


model15 <- lm(TotalAmt~ card2items+
                carditems+ 
                income + card+card2+gender+
                region +townsize+
                multline +ownpc + card2fee+ response_03+
                age+ cardbenefit+owndvd 
              +voice,
              data = dev.CooksDist)
summary(model15)


# Useful parameters that drive total spend of credit card(Primary Card + Secondary card)
card2items
carditems
income
card
card2
gender
region
townsize
multline
ownpc
card2fee
response_03
age
cardbenefit
owndvd
voice