setwd("C:/Users/shash/Data")

Bank_train= read.csv("bank-full_train.csv", stringsAsFactors = F)
Bank_test= read.csv("bank-full_test.csv", stringsAsFactors = F)

#adding variable y with inputs NA
Bank_test$y=NA

Bank_train$data='train'
Bank_test$data='test'

#Binding both the dataset
Bank=rbind(Bank_train,Bank_test)

library(dplyr)

glimpse(Bank)

Bank=Bank %>% 
  mutate(pdays=ifelse(pdays==-1,0,substr(pdays,1,3)),
         pdays=as.numeric(pdays))

lapply(Bank,function(x) sum(is.na(x)))

################################

lapply(Bank,function(x) length(unique(x)))

names(Bank)[sapply(Bank,function(x) is.character(x))]

#creating function for dummy variables
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

#checking all the character variables
names(Bank)[sapply(Bank,function(x) is.character(x))]

# excluded y and data variables
cat_cols=c("job","marital","education","default","housing","loan",
           "contact","poutcome","month")

for(cat in cat_cols){
  Bank=CreateDummies(Bank,cat,50)
}

#converting y into binary
Bank$y=as.numeric(Bank$y=='yes')

glimpse(Bank)

#separating train and test data
Bank_train=Bank %>% filter(data=='train') %>% select(-data)
Bank_test=Bank %>% filter(data=='test') %>% select (-data,-y)

#sampling train into 2 parts for validation 
set.seed(2)
s=sample(1:nrow(Bank_train),0.8*nrow(Bank_train))
Banktrain1=Bank_train[s,]
Banktest2=Bank_train[-s,]

library(car)

#applying linear regression
for_vif_test=lm(y~.-ID,data=Banktrain1)

sort(vif(for_vif_test),decreasing = T)[1:3]

for_vif_test=lm(y~.-ID-month_may
                -job_blue_collar-poutcome_unknown
                -education_secondary-contact_unknown,
                data=Banktrain1)

sort(vif(for_vif_test),decreasing = T)[1:3]

#applying logistic regression
log_fit_test=glm(y~.-ID-month_may
                 -job_blue_collar-poutcome_unknown
                 -education_secondary-contact_unknown,
                 data=Banktrain1, family="binomial")

summary(log_fit_test)

log_fit_test=step(log_fit_test)

formula(log_fit_test)

log_fit_test=glm(y ~ duration + campaign + pdays + previous + 
                   job_management + marital_single + marital_married + 
                   housing_yes + loan_no + 
                   contact_cellular + poutcome_other + poutcome_failure +
                   month_mar + month_sep + month_oct + month_apr
                 ,data=Bank_train, family="binomial")

summary(log_fit_test)


library(pROC)

#prediction on 20% of train data
val.score=predict(log_fit_test,newdata = Banktest2,type='response')
auc.score=auc(roc(Banktest2$y,val.score))
auc.score

library(ggplot2)

mydata=data.frame(y=Banktest2$y,val.score=val.score)
#ploting predicted against actual value
ggplot(mydata,aes(y=y,x=val.score,color=factor(y)))+
  geom_point()+geom_jitter()

########################
#applying linear regression on complete train data
for_vif = lm(
  y ~ . - ID - month_may
  - job_blue_collar - poutcome_unknown
  - education_secondary - contact_unknown,
  data = Bank_train
)

sort(vif(for_vif), decreasing = T)[1:3]

#applying logistic regression on complete train data
log_fit_final = glm(
  y ~ . - ID - month_may
  - job_blue_collar - poutcome_unknown
  - education_secondary - contact_unknown,
  data = Bank_train,
  family = "binomial"
)


log_fit_final = step(log_fit_final)

formula(log_fit_final)

log_fit_final = glm(
  y ~ duration + campaign + pdays + previous +
    marital_single + marital_married +
    housing_yes  +
    contact_cellular + poutcome_other + poutcome_failure + month_mar +
    month_sep + month_oct + month_jan + month_apr
  ,
  data = Bank_train,
  family = "binomial"
)

summary(log_fit_final)

#predicted value on test data
test.score = predict(log_fit_final, newdata = Bank_test, type = 'response')

#to get values in the form of binary
real = Bank_train$y
cutoffs = seq(0.001, 0.999, 0.001)
cutoff_data = data.frame(
  cutoff = 99,
  Sn = 99,
  Sp = 99,
  KS = 99,
  F5 = 99,
  F.1 = 99,
  M = 99
)

for (cutoff in cutoffs) {
  predicted = as.numeric(test.score > cutoff)
  TP = sum(real == 1 & predicted == 1)
  TN = sum(real == 0 & predicted == 0)
  FP = sum(real == 0 & predicted == 1)
  FN = sum(real == 1 & predicted == 0)
  P = TP + FN
  N = TN + FP
  Sn = TP / P
  Sp = TN / N
  precision = TP / (TP + FP)
  recall = Sn
  KS = (TP / P) - (FP / N)
  F5 = (26 * precision * recall) / ((25 * precision) + recall)
  F.1 = (1.01 * precision * recall) / ((.01 * precision) + recall)
  M = (4 * FP + FN) / (5 * (P + N))
  cutoff_data = rbind(cutoff_data, c(cutoff, Sn, Sp, KS, F5, F.1, M))
}
cutoff_data = cutoff_data[-1, ]

library(ggplot2)
#ROC curve
ggplot(cutoff_data, aes(x = cutoff, y = Sp)) + geom_line()

library(tidyr)
cutoff_long = cutoff_data %>%
  gather(Measure, Value, Sn:M)
ggplot(cutoff_long, aes(x = cutoff, y = Value, color = Measure)) + geom_line()

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

#final predicted cutoff
test.predicted = as.numeric(test.score > my_cutoff)
write.csv(test.predicted, "finalbinaryvalues.csv", row.names = F)
