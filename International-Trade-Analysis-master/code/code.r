RMSE.NN
rsq <- cor(predict_testNN, datatest$tradeflow_comtrade_o)^2
rsq
plot(NN)
NN = neuralnet(tradeflow_comtrade_o ~ gdpcap_o + gdpcap_d +distwces + contig + comlang_off + comlang_ethno + comcol + rta +comrelig+ rta_coverage + rta_type + wto_o + wto_d , trainNN, hidden = c(10,10,5) , linear.output = F,algorithm = "rprop+",learningrate=0.3 )
plot(NN)
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df_1$tradeflow_comtrade_o))) + min(df_1$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
rsq <- cor(predict_testNN, datatest$tradeflow_comtrade_o)^2
rsq
NN = neuralnet(tradeflow_comtrade_o ~ gdpcap_o + gdpcap_d +distwces + contig + comlang_off + comlang_ethno + comcol + rta +comrelig+ rta_coverage + rta_type + wto_o + wto_d , trainNN, hidden = c(15,10,5) , linear.output = F,algorithm = "rprop+",learningrate=0.5 )
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df_1$tradeflow_comtrade_o))) + min(df_1$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
NN = neuralnet(tradeflow_comtrade_o ~ gdpcap_o + gdpcap_d +distwces + contig + comlang_off + comlang_ethno + comcol + rta +comrelig+ rta_coverage + rta_type + wto_o + wto_d , trainNN, hidden = c(15,10,5) , linear.output = F,algorithm = "rprop+",learningrate=0.01 )
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df_1$tradeflow_comtrade_o))) + min(df_1$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
rsq <- cor(predict_testNN, datatest$tradeflow_comtrade_o)^2
rsq
NN = neuralnet(tradeflow_comtrade_o ~ gdpcap_o + gdpcap_d +distwces + contig + comlang_off + comlang_ethno + comcol + rta +comrelig+ rta_coverage + rta_type + wto_o + wto_d , trainNN, hidden = c(15,10,5) , linear.output = F,algorithm = "rprop+",learningrate=0.001 )
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df_1$tradeflow_comtrade_o))) + min(df_1$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
rsq <- cor(predict_testNN, datatest$tradeflow_comtrade_o)^2
rsq
NN = neuralnet(tradeflow_comtrade_o ~ gdpcap_o + gdpcap_d +distwces + contig + comlang_off + comlang_ethno + comcol + rta +comrelig+ rta_coverage + rta_type + wto_o + wto_d , trainNN, hidden = c(15,15,10) , linear.output = F,algorithm = "rprop+",learningrate=0.0001 )
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df_1$tradeflow_comtrade_o))) + min(df_1$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
NN = neuralnet(tradeflow_comtrade_o ~ gdpcap_o + gdpcap_d +distwces + contig + comlang_off + comlang_ethno + comcol + rta +comrelig+ rta_coverage + rta_type + wto_o + wto_d , trainNN, hidden = c(15,10,5) , linear.output = F,algorithm = "rprop+",learningrate=0.001 )
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df_1$tradeflow_comtrade_o))) + min(df_1$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
plot(NN)
plot(NN)
write.csv(df_1,"sixth_try.csv")
library(readr)
Gravity_V202102 <- read_csv("Gravity_V202102.csv")
View(Gravity_V202102)
library(readr)
Gravity_V202102 <- read_csv("Gravity_V202102.csv")
View(Gravity_V202102)
library(readr)
final_combined_data <- read_csv("final_combined_data.csv")
View(final_combined_data)
summary(final_combined_data)
library(readr)
secondtry <- read_csv("secondtry.csv")
View(secondtry)
summary(secondtry)
library(readr)
one_more_try <- read_csv("one_more_try.csv")
View(one_more_try)
summary(one_more_try)
head(one_more_try)
df <- head(one_more_try)
library(readr)
sixth_try <- read_csv("sixth_try.csv")
View(sixth_try)
df<- sixth_try
data <- sixth_try
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
datatrain = data[ index, ]
datatest = data[ -index, ]
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
library(neuralnet)
trainNN = scaled[index , ]
testNN = scaled[-index , ]
testNN = scaled[-index , ]
set.seed(2)
NN = neuralnet(tradeflow_comtrade_o ~ distwces + contig + comlang_off + comcol + comlang_ethno + comrelig + gdpcap_o + gdpcap_d + gatt_o + gatt_d + wto_o + wto_d + rta + rta_coverage + rta_type, trainNN, hidden = c(15,10,5) , linear.output = F, algorithm = "rprop+" ,learningrate=0.001)
NN = neuralnet(tradeflow_comtrade_o ~ distwces + contig + comlang_off + comcol + comlang_ethno + comrelig + gdpcap_o + gdpcap_d + wto_o + wto_d + rta + rta_coverage + rta_type, trainNN, hidden = c(15,10,5) , linear.output = F, algorithm = "rprop+" ,learningrate=0.001)
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(data$tradeflow_comtrade_o) - min(data$tradeflow_comtrade_o))) + min(data$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
NN = neuralnet(tradeflow_comtrade_o ~ distwces + contig + comlang_off + comcol + comlang_ethno + comrelig + gdpcap_o + gdpcap_d + wto_o + wto_d + rta + rta_coverage + rta_type, trainNN, hidden = c(10,10,5) , linear.output = F, algorithm = "rprop+" ,learningrate=0.01)
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(data$tradeflow_comtrade_o) - min(data$tradeflow_comtrade_o))) + min(data$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
NN = neuralnet(tradeflow_comtrade_o ~ distwces + contig + comlang_off + comcol + comlang_ethno + comrelig + gdpcap_o + gdpcap_d + wto_o + wto_d + rta + rta_coverage + rta_type, trainNN, hidden = c(10,10,5) , linear.output = F, algorithm = "rprop+" ,learningrate=0.1)
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(data$tradeflow_comtrade_o) - min(data$tradeflow_comtrade_o))) + min(data$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
NN = neuralnet(tradeflow_comtrade_o ~ distwces + contig + comlang_off + comcol + comlang_ethno + comrelig + gdpcap_o + gdpcap_d + wto_o + wto_d + rta + rta_coverage + rta_type, trainNN, hidden = c(10,10,5) , linear.output = F, algorithm = "rprop+" ,learningrate=0.5)
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(data$tradeflow_comtrade_o) - min(data$tradeflow_comtrade_o))) + min(data$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
rsq <- cor(predict_testNN,datatest$tradeflow_comtrade_o)^2
rsq
rs <- head(predict_testNN)
act <- head(datatest$tradeflow_comtrade_o)
rs
act
library(VIM)
install.packages("VIM")
library(VIM)
df<- sixth_try
kNN(df,variable = c("distwces","gdpcap_o"))
library(readr)
sorted_one_more <- read_csv("sorted_one_more.csv")
View(sorted_one_more)
kNN(df,variable = c("distwces","gdpcap_o"))
kNN(sorted_one_more,variable = c("distwces","gdpcap_o"))
library(readr)
sixth_try <- read_csv("sixth_try.csv")
View(sixth_try)
install.packages("randomForest")
library(randomForest)
tradeflow_comtrade_o.rf <- randomForest(tradeflow_comtrade_o ~ ., data = sixth_try, mtry = 3,
importance = TRUE, na.action = na.omit)
print(tradeflow_comtrade_o.rf)
install.packages("rpart")
library(rpart)
fit <- rpart(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d,
method = "anova", data = sixth_try )
print(fit)
install.packages("e1071")
library(e1071)
modelsvm = svm(tradeflow_comtrade_o ~ distwces + gdpcap_o,sixth_try)
predYsvm = predict(modelsvm, sixth_try)
RMSEsvm=rmse(predYsvm,sixth_try$tradeflow_comtrade_o)
library(rmse)
library(metrics)
library(Metrics)
install.packages("Metrics")
library(Metrics)
RMSEsvm=rmse(predYsvm,sixth_try$tradeflow_comtrade_o)
RMSEsvm
modelsvm = svm(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d + contig + comlang_off + comlang_ethno + rta+ rta_coverage + rta_type + wto_o + wto_d + comrelig ,sixth_try)
predYsvm = predict(modelsvm, sixth_try)
RMSEsvm=rmse(predYsvm,sixth_try$tradeflow_comtrade_o)
RMSEsvm
library(readr)
Gravity_V202102 <- read_csv("Gravity_V202102.csv")
View(Gravity_V202102)
df <- Gravity_V202102[Gravity_V202102$iso3num_o==c("IND","USA","BGD","CHN","FRA","BEL","ITA","GBR","DEU","HKG","MMR","JPN","PAK","KOR","ARE","SAU","SGP","CHE"),]
df
df <- Gravity_V202102[Gravity_V202102$iso3_o==c("IND","USA","BGD","CHN","FRA","BEL","ITA","GBR","DEU","HKG","MMR","JPN","PAK","KOR","ARE","SAU","SGP","CHE"),]
df
summary(df)
df$iso3num_d[is.na(df$iso3num_d)] <- 429.6
View(df)
df$contig[is.na(df$contig)] <- 0.02
df$distwces[is.na(df$distwces)] <- 8109.43
df$comlang_off[is.na(df$comlang_off)] <- 0.1715
df$comlang_ethno[is.na(df$comlang_ethno)] <- 0.1931
df$comcol[is.na(df$comcol)] <- 0.1142
df$comrelig[is.na(df$comrelig)] <- 0.141
df$gdpcap_o[is.na(df$gdpcap_o)] <- 14.949
df$gdpcap_d[is.na(df$gdpcap_d)] <- 7.084
df$gatt_o[is.na(df$gatt_o)] <- 0.7971
df$gatt_d[is.na(df$gatt_d)] <- 0.3971
df$wto_o[is.na(df$wto_o)] <- 0.3478
df$wto_d[is.na(df$wto_d)] <- 0.2271
df$rta[is.na(df$rta)] <- 0.0815
df$rta_coverage[is.na(df$rta_coverage)] <- 0.1391
df$rta_type[is.na(df$rta_type)] <- 5.847
df$tradeflow_comtrade_o[is.na(df$tradeflow_comtrade_o)] <- 1124661
df <- df[,c("iso3num_o","iso3num_d","contig","comlang_off","comlang_ethno","distwces","comcol","comrelig","gdpcap_o","gdpcap_d","gatt_o","gatt_d","wto_o","wto_d","rta","rta_type","rta_coverage","tradeflow_comtrade_o")]
View(df)
samplesize = 0.60 * nrow(df)
set.seed(80)
index = sample( seq_len ( nrow ( df ) ), size = samplesize )
datatrain = df[ index, ]
datatest = df[ -index, ]
max = apply(df , 2 , max)
min = apply(df, 2 , min)
scaled = as.data.frame(scale(df, center = min, scale = max - min))
library(neuralnet)
trainNN = scaled[index , ]
testNN = scaled[-index , ]
set.seed(2)
NN = neuralnet(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d + contig + comlang_off + comlang_ethno + comcol + comrelig + rta + rta_type + rta_coverage + wto_o + wto_d, trainNN, hidden = c(15,10) , linear.output = F,algorithm = "rprop+",learningrate = 0.01 )
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df$tradeflow_comtrade_o))) + min(df$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
NN = neuralnet(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d + contig + comlang_off + comlang_ethno + comcol + comrelig + rta + rta_type + rta_coverage + wto_o + wto_d, trainNN, hidden = c(15,10) , linear.output = F,algorithm = "rprop+",learningrate = 0.001 )
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(df$tradeflow_comtrade_o) - min(df$tradeflow_comtrade_o))) + min(df$tradeflow_comtrade_o)
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
library(readr)
final_combined_data <- read_csv("final_combined_data.csv")
View(final_combined_data)
summary(final_combined_data)
library(readr)
final_sorted <- read_csv("final_sorted.csv")
View(final_sorted)
summary(final_sorted)
library(readr)
one_more_try_2 <- read_csv("one_more_try_2.csv")
View(one_more_try_2)
summary(one_more_try_2)
library(readr)
third_try <- read_csv("third_try.csv")
View(third_try)
sum(third_try)
summary(third_try)
library(readr)
fourth_try <- read_csv("fourth_try.csv")
View(fourth_try)
tradeflow_comtrade_o.rf <- randomForest(tradeflow_comtrade_o ~ ., data = trainNN, mtry = 3,
importance = TRUE)
plot(tradeflow_comtrade_o.rf)
print(tradeflow_comtrade_o.rf)
tradeflow_comtrade_o.rf <- randomForest(tradeflow_comtrade_o ~ ., data = sixth_try, mtry = 3,
importance = TRUE)
print(tradeflow_comtrade_o.rf)
plot(tradeflow_comtrade_o.rf)
fit <- rpart(tradeflow_comtrade_o ~ distwces +
gdpcap_o + gdpcap_d ,
method = "anova", data = sixth_try)
plot(fit)
print(fit)
plot(trainNN)
modelsvm = svm(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d + contig + comcol + comrelig + comlang_off + comlang_ethno + rta + rta_type + rta_coverage + wto_o + wto_d ,trainNN)
predYsvm = predict(modelsvm, testNN)
RMSEsvm=rmse(predYsvm,datatest$tradeflow_comtrade_o)
RMSE.NN
View(datatest)
library(readr)
sixth_try <- read_csv("sixth_try.csv")
View(sixth_try)
df <- sixth_try
samplesize = 0.60 * nrow(df)
set.seed(80)
index = sample( seq_len ( nrow ( df ) ), size = samplesize )
datatrain = df[ index, ]
datatest = df[ -index, ]
max = apply(data , 2 , max)
max = apply(df , 2 , max)
min = apply(df, 2 , min)
scaled = as.data.frame(scale(df, center = min, scale = max - min))
trainNN = scaled[index , ]
testNN = scaled[-index , ]
library(e1071)
modelsvm = svm(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d + contig + comcol + comrelig + comlang_off + comlang_ethno + rta + rta_type + rta_coverage + gatt_o + gatt_d + wto_o + wto_d ,trainNN)
modelsvm = svm(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d + contig + comcol + comrelig + comlang_off + comlang_ethno + rta + rta_type + rta_coverage  + wto_o + wto_d ,trainNN)
predYsvm = predict(modelsvm, testNN)
RMSEsvm=rmse(predYsvm,datatest$tradeflow_comtrade_o)
RMSEsvm
predYsvm = predict(modelsvm, trainNN)
RMSEsvm=rmse(predYsvm,datatest$tradeflow_comtrade_o)
RMSEsvm
modelsvm = svm(tradeflow_comtrade_o ~ distwces + gdpcap_o + gdpcap_d + contig + comcol + comrelig + comlang_off + comlang_ethno + rta + rta_type + rta_coverage  + wto_o + wto_d ,df)
predYsvm = predict(modelsvm,df)
predYsvm = predict(modelsvm, df$tradeflow_comtrade_o)
predYsvm = predict(modelsvm, df$tradeflow_comtrade_o)
RMSEsvm=rmse(predYsvm,df$tradeflow_comtrade_o)
RMSEsvm
library(randomForest)
trade.rf <- randomForest(tradeflow_comtrade_o ~ ., data = df, mtry = 5 ,
importance = TRUE)
plot(trade.rf)
print(trade.rf)
trade.rf <- randomForest(tradeflow_comtrade_o ~ ., data = trainNN, mtry = 5 ,
importance = TRUE)
trade.rf <- randomForest(tradeflow_comtrade_o ~ ., data = trainNN, mtry = 5 ,
importance = TRUE,na.action = na.omit)
trade.rf <- randomForest(tradeflow_comtrade_o ~ ., data = df, mtry = 5 ,
importance = TRUE,na.action = na.omit)
y_pred = predict(classifier, newdata = df)
y_pred = predict(trade.rf, newdata = df)
cm = table(df, y_pred)
y_pred = predict(trade.rf, newdata = df$tradeflow_comtrade_o)
y_pred = predict(trade.rf$predicted,newdata = df$tradeflow_comtrade_o)
trade.rf <- randomForest(tradeflow_comtrade_o ~ ., data = df, mtry = 6 ,
importance = TRUE,na.action = na.omit)
print(trade.rf)
trade.rf <- randomForest(tradeflow_comtrade_o ~ distwces + gdpcap_o, data = df, mtry = 6 ,
importance = TRUE,na.action = na.omit)
trade.forest <- randomForest(tradeflow_comtrade_o ~ distwces + gdpcap_o, data = df, mtry = 6 ,
importance = TRUE,na.action = na.omit)
trade.forest <- randomForest(tradeflow_comtrade_o ~., data = df, mtry = 6 ,
importance = TRUE,na.action = na.omit)
print(trade.forest)
trade <- randomForest(tradeflow_comtrade_o ~., data = df, mtry = 6 ,
importance = TRUE,na.action = na.omit)
trade
trade <- randomForest(tradeflow_comtrade_o ~., data = df, mtry = 6)
trade
trade <- randomForest(tradeflow_comtrade_o ~., data = df, mtry = 6,do.Trace=T)
trade
trade <- randomForest(tradeflow_comtrade_o ~., data = trainNN, mtry = 6)
trade <- randomForest(tradeflow_comtrade_o ~., data = trainNN, mtry = 6,na.action = na.omit)
trade <- randomForest(tradeflow_comtrade_o ~., data = datatrain, mtry = 6,na.action = na.omit)
y_pred = predict(trade, newdata = datatest)
cm = table(datatest, y_pred)
y_pred = predict(trade, newdata = datatest$tradeflow_comtrade_o)
y_pred = predict(trade$predicted, newdata = datatest$tradeflow_comtrade_o)
trade$rsq
mean(trade$rsq)
trade$oob.times
plot(predict(trade),df)
plot(predict(trade,newdata = df),df)
x <- df[,1:18]
View(trade.forest)
y <- df[,19]
rf <- randomForest(x,y)
View(sixth_try)
library(readr)
fifth_try <- read_csv("fifth_try.csv")
View(fifth_try)
library(gravity)
df <- fifth_try
fit <- bvu(dependent_variable = "tradeflow_comtrade_o",distance = "distwces",additional_regressors = c("rta","contig"),income_origin = "gdp_o",income_destination = "gdp_d",code_origin = "iso3num_o",code_destination = "iso3num_d",data = df)
fit <- bvw(dependent_variable = "tradeflow_comtrade_o",distance = "distwces",additional_regressors = c("rta","contig"),income_origin = "gdp_o",income_destination = "gdp_d",code_origin = "iso3num_o",code_destination = "iso3num_d",data = df)
fit <- ddm(dependent_variable = "tradeflow_comtrade_o",distance = "distwces",additional_regressors = c("rta","contig"),code_origin = "iso3num_o",code_destination = "iso3num_d",data = df)
fit <- tobit(dependent_variable = "tradeflow_comtrade_o",distance = "distwces",additional_regressors = c("rta","contig"),data = df)
fit
fit <- tobit(dependent_variable = "tradeflow_comtrade_o",distance = "distwces",additional_regressors = c("rta","contig"),added_constant = 1 , data = df)
fit
fit <- fixed_effects(dependent_variable = "tradeflow_comtrade_o",distance = "distwces",additional_regressors = c("rta","contig"),code_origin = "iso3num_o",code_destination = "iso3num_d",data = df)
fit
View(final_sorted)
Gravity_V202102 <- readRDS("~/Gravity_V202102.Rds")
Gravity_V202102
View(Gravity_V202102)
Gravity_V202102 <- readRDS("~/Gravity_V202102.Rds")
View(Gravity_V202102)
df <- Gravity_V202102
df <- df[iso3_o="IND",]
df1 <- df[iso3_o="IND"]
df1 <- as.data.frame(df[iso3_o="IND",])
df1 <- as.data.frame(df[c(iso3_o="IND"),])
View(df1)
df1 <- df[iso3_o=="IND",]
df1 <- df[c(iso3_o=="IND"),]
df1 <- df[df$iso3_o=="IND",]
df1
View(df1)
df2 <- df1[df1$iso3_d=="USA",]
View(df2)
View(df2)
summary(df1)
df2 <- df1[df1$iso3_d==c("USA","JPN"),]
df2 <- df1[df1$iso3_d==("USA","JPN"),]
df2 <- subset[df1, df1$iso3_d==("USA","JPN")]
df2 <- subset[df1, df1$iso3_d==c("USA","JPN")]
df2 <- as.data.frame()[df1, df1$iso3_d==c("USA","JPN")]
df2 <- as.data.frame(df1, df1$iso3_d==c("USA","JPN"))
df2 <- df1[c(df1$iso3_d=="USA",df1$iso3_d=="JPN"),]
df1 <- df[c(df$iso3_o=="USA",df$iso3_o=="JPN",df$iso3_o=="CHN",df$iso3_o=="ARE",df$iso3_o=="SGP",df$iso3_o=="HKG",df$iso3_o=="GBR",df$iso3_o=="DEU",df$iso3_o=="KOR",df$iso3_o=="FRA",df$iso3_o=="SAU",df$iso3_o=="BGD",df$iso3_o=="PAK",df$iso3_o=="IND",df$iso3_o=="CHE",df$iso3_o=="MMR",df$iso3_o=="ITA"),]
View(df1)
View(df2)
df2 <- df[c(df$iso3_d==c("USA","JPN")),]
df2 <- df[c(df$iso3_o==c("USA","JPN")),]
df2 <- df[c(df$iso3_o=="USA",]
df2 <- df[c(df$iso3_o=="USA"),]
df2 <- df[c(df$iso3_o==c("USA","IND")),]
df2 <- df[c(df$iso3_o=="USA"+"IND"),]
df2 <- df[c(df$iso3_o=="USA"+df$iso3_o=="IND"),]
df2 <- df[c(df$iso3_o=="USA",df$iso3_o=="IND"),]
df2 <- df[df$iso3_o==c("USA","IND"),]
df2 <- df[df$iso3_o==c("USA"),]
df3 <- df2[df2$iso3_d==c("IND"),]
View(df3)
summary(df)
df3  <- na.omit(df)
df3 <- df[iso3_o=="IND",]
df3 <- df[iso3_o==c("IND"),]
df3 <- df[df$iso3_o==c("IND"),]
summary(df3)
df3 <- df3[df$iso3_d==c("USA"),]
df3 <- df[df$iso3_d==c("IND"),]
df3 <- df[df$iso3_o==c("IND"),]
df3 <- df3[df3$iso3_d==c("USA"),]
write.csv(df3,file="data101")
write.csv(df3.csv,file="data101")
write.csv(df3,file="data101.csv")
Gravity_V202102 <- readRDS("~/Gravity_V202102.Rds")
df <-  Gravity_V202102
library(readr)
sixth_try <- read_csv("sixth_try.csv")
View(sixth_try)
samplesize = 0.60 * nrow(sixth_try)
set.seed(80)
index = sample( seq_len ( nrow ( sixth_try ) ), size = samplesize )
datatrain = sixth_try[ index, ]
datatest = sixth_try[ -index, ]
max = apply(sixth_try , 2 , max)
min = apply(sixth_try, 2 , min)
scaled = as.data.frame(scale(sixth_try, center = min, scale = max - min))
library(neuralnet)
trainNN = scaled[index , ]
testNN = scaled[-index , ]
set.seed(2)
NN = neuralnet(tradeflow_comtrade_o ~ contig + distwces + comlang_off + comcol + comrelig , trainNN, hidden = c(10,5) , linear.output = F ,algorithm = "rprop+" )
plot(NN)
predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(sixth_try$tradeflow_comtrade_o) - min(sixth_try$tradeflow_comtrade_o))) + min(sixth_try$tradeflow_comtrade_o)
plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted  NN", xlab = "real ")
plot(datatest$tradeflow_comtrade_o, predict_testNN, col='blue', pch=16, ylab = "predicted  NN", xlab = "real ")
RMSE.NN = (sum((datatest$tradeflow_comtrade_o - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
rsq <- cor(predict_testNN,datatest$tradeflow_comtrade_o)^2
rsq
Gravity_V202102 <- readRDS("~/Gravity_V202102.Rds")
library(readr)
rows_1 <- read_csv("~/rows_1.csv")
View(rows_1)
df <- rows_1
df_1 <- df[,c("year","iso3_o","iso3_d","iso3num_o","iso3num_d","contig","distwces","comlang_off","comlang_ethno","comcol","comrelig","pop_o","pop_d","gdp_o","gdp_d","gdpcap_o","gdpcap_d","gatt_o","gatt_d","wto_o","wto_d","eu_o","eu_d","rta","rta_coverage","rta_type","entry_cost_o","entrycost_d","tradeflow_comtrade_o","tradeflow_imf_o")]
df_1 <- df[,c("year","iso3_o","iso3_d","iso3num_o","iso3num_d","contig","distwces","comlang_off","comlang_ethno","comcol","comrelig","pop_o","pop_d","gdp_o","gdp_d","gdpcap_o","gdpcap_d","gatt_o","gatt_d","wto_o","wto_d","eu_o","eu_d","rta","rta_coverage","rta_type","entry_cost_o","entry_cost_d","tradeflow_comtrade_o","tradeflow_imf_o")]
View(df_1)
library(gravity)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
fit
summary(fit)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','pop_o','pop_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary()
summary(fit)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- fixed_effects(dependent_variable = log('tradeflow_comtrade_o'),distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
fit <- fixed_effects(dependent_variable = log(tradeflow_comtrade_o),distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
fit <- fixed_effects(dependent_variable = log(df_1$tradeflow_comtrade_o),distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
fit <- fixed_effects(dependent_variable = 'log(df_1$tradeflow_comtrade_o)',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- ppml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d'),data = df_1)
summary(fit4)
summary(fit)
fit <- ppml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
summary(fit)
fit <- gpml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
summary(gpml)
fit <- gpml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
summary(fit)
fit <- nbpml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
summary(fit)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','gdp_o','gdp_d'),data = df_1)
summary(fit)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','pop_o','pop_d','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','gdp_o','gdp_d'),data = df_1)
df_2 <- df_1 %>%
mutate(lgdp_o=log(gdp_o),lgdp_d=log(gdp_d))
df_2 <- df_1
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','gdp_o','gdp_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','comlang_ethno','entry_cost_o','entry_cost_d','gdp_o','gdp_d','iso3_o','iso3_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','comrelig','rta','rta_coverage','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','comlang_off','entry_cost_o','entry_cost_d','gdp_o','gdp_d','iso3_o','iso3_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol'),data = df_1)
summary(fit)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d'),data = df_1)
summary(fit)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig','comlang_ethno'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig','comlang_ethno','comlang_off'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig','comlang_ethno','comlang_off','gatt_o'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig','comlang_ethno','comlang_off','gatt_o','gatt_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig','comlang_ethno','comlang_off','gatt_o','eu_o'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig','comlang_ethno','comlang_off','gatt_o','eu_o','eu_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('contig','comcol','iso3_o','iso3_d','rta','rta_type','comrelig','comlang_ethno','comlang_off','gatt_o','eu_o','wto_o'),data = df_1)
summary(fit)
fit <- ols(dependent_variable = 'log(df_1$tradeflow_comtrade_o)',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),income_origin = 'gdp_o',income_destination='gdp_d',code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
fit <- ols(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),income_origin = 'gdp_o',income_destination='gdp_d',code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- ols(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),income_origin = 'gdp_o',income_destination='gdp_d',code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d','rta_type'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- ols(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),income_origin = 'gdp_o',income_destination='gdp_d',code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- fixed_effects(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d'),code_origin = 'iso3_o',code_destination = 'iso3_d',data = df_1)
summary(fit)
fit <- ppml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
summary(fit)
fit <- gpml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
summary(fit)
fit <- nbpml(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
summary(fit)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','gatt_d','wto_o','wto_d','eu_o','eu_d','pop_o','pop_d','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','wto_o','eu_o','pop_o','pop_d','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','wto_o','eu_o','pop_o','entry_cost_o','entry_cost_d','iso3_o','iso3_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','wto_o','eu_o','pop_o','entry_cost_o','iso3_o','iso3_d'),data = df_1)
fit <- nls(dependent_variable = 'tradeflow_comtrade_o',distance = 'distwces',additional_regressors = c('rta','rta_coverage','contig','comlang_off','rta_type','comlang_ethno','comcol','comrelig','gatt_o','wto_o','eu_o','pop_o','entry_cost_o','iso3_o'),data = df_1)
View(df_1)
df_3 <- subset(df_1, select = -'tradeflow_imf_o' )
df_3 <- subset(df_1, select = -tradeflow_imf_o )
df_3 <- subset(df_1, select = -pop_o )
df_3 <- subset(df_1, select = -pop_d )
df_3 <- subset(df_1, select = -c(pop_d) )
df_3 <- subset(df_1, select = -c(pop_d,tradedlow_imf_o) )
df_3 <- subset(df_1, select = -c(pop_d,tradeflow_imf_o) )
df_3 <- subset(df_1, select = -c(pop_d,tradeflow_imf_o,pop_o,gdpcap_o,gdpcap_d) )
write.csv(df_3,'final_dataset_research')
write.csv(df_3,'final_dataset_research.csv')
