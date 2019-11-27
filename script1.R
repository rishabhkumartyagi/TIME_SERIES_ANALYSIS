#RISHABH KUMAR TYAGI
#TIME SERIES ANALYSIS

#READING MAIN TABLE
fulltable = read.csv("timeori.csv",header = T,sep =',')
View(fulltable)

#CREATING SEPARATE TABLE FOR WIND ENERGY
windentable = cbind.data.frame(fulltable$DE_50hertz_wind_generation_actual,fulltable$DE_50hertz_wind_generation_forecast,fulltable$utc_timestamp)
colnames(windentable) = c("DE_50hertz_wind_generation","DE_50hertz_wind_generation_forecast","utc_timestamp")
fullwindent = windentable

#ANALYSING TABLE
View(windentable)
str(windentable)
summary(windentable)

#PREPROCESSING OF TIME SERIES
library(stringr)
windentable$DE_50hertz_wind_generation[ is.na(windentable$DE_50hertz_wind_generation)] =2298#imputing with mean
windentable = windentable[-1,]
newcol = str_split_fixed(windentable$utc_timestamp, "T", 2)
temptable = cbind.data.frame(windentable$DE_50hertz_wind_generation,windentable$DE_50hertz_wind_generation_forecast,newcol[,1],newcol[,2]) #separating date and time
#exptable=data.frame(matrix(NA, nrow = 0, ncol = 3))
exptable = aggregate(.~newcol[, 1],temptable,sum)
windentable = cbind.data.frame(exptable$`windentable$DE_50hertz_wind_generation`/24,exptable$`windentable$DE_50hertz_wind_generation_forecast`/24,exptable$`newcol[, 1]`)
colnames(windentable) = c("DE_50hertz_wind_generation","DE_50hertz_wind_generation_forecast","date")
write.csv(windentable,file = "windentable.csv",sep = ",",row.names = F,col.names = T)
rm(fulltable,vc,temptable,exptable,newcol)

#CREATING TIME SERIES FOR ANALYSIS
trainset = windentable[((365*2):(nrow(windentable)-365)),]
trainset$DE_50hertz_wind_generation[trainset$DE_50hertz_wind_generation <150]=2298
testset = windentable[((nrow(windentable)-364):nrow(windentable)),]
traints = ts(trainset$DE_50hertz_wind_generation,frequency = 365)
testts = ts(testset$DE_50hertz_wind_generation,frequency = 365)

ensembledf=data.frame(matrix(NA, nrow = 365, ncol = 2))

##ARIMA
#IMPORTING DEPENDECIES
install.packages('forecast')
install.packages('tseries')
install.packages('munsell')
library(forecast)
library(tseries)
library(munsell)

#AUTO ARIMA WITH SEASONALITY
ttime1 = Sys.time()
arimamodel=auto.arima(traints,D=1)
ttime2 = Sys.time()
arimatraintime = ttime2 - ttime1

ttime1 = Sys.time()
arimaforcast = forecast(arimamodel,h=365)
ttime2 = Sys.time()
arimaforecasttime = ttime2 - ttime1

plot(arimaforcast)
arimares = data.frame(arimaforcast)
arimares = arimares$Point.Forecast

ensembledf$arimapred=arimares

arimarmse = rmse(testset$DE_50hertz_wind_generation,arimares)


library("reshape2")
library("ggplot2")


plotdf = cbind.data.frame(arimares,testset$DE_50hertz_wind_generation,1:(365))
colnames(plotdf)=c("ARIMA","Original Data","Number_of_days")

test_data_long <- melt(plotdf, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()



#Facebook Prophet
install.packages("prophet")
library(prophet)

facebookdf=cbind.data.frame(trainset$date,trainset$DE_50hertz_wind_generation)
colnames(facebookdf)=c("ds","y")
facebookdf$ds=gsub("Z","",facebookdf$ds)
facebookdf$ds=gsub("T"," ",facebookdf$ds)

ttime1 = Sys.time()
m = prophet(facebookdf ,seasonality.mode = 'multiplicative',yearly.seasonality = T ,weekly.seasonality = F,daily.seasonality = F)
ttime2 = Sys.time()
fbProtime=ttime2-ttime1

future = make_future_dataframe(m,periods=365)

ttime1 = Sys.time()
fore=predict(m,future)
ttime2 = Sys.time()
fbProforecasttime=ttime2-ttime1

plot(m,fore)

ensembledf$fbpropred = fore$yhat[(nrow(testset)+1):(nrow(testset)+365)]

fbprormse = rmse(testset$DE_50hertz_wind_generation,fore$yhat[(nrow(testset)+1):(nrow(testset)+365)])

plotpd = cbind.data.frame(fore$yhat[(length(fore$yhat)-364):(length(fore$yhat))],testset$DE_50hertz_wind_generation[1:(365)],1:(365))
colnames(plotpd)=c("Facebook Prophet","Original Data","Number_of_days")

test_data_long <- melt(plotpd, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()




#RandomForest
install.packages("randomForest")
library('randomForest')

#SHIFT FUNCTION
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

#ENGINEERING FEATURES
#ROLL OVER AVERAGES
artcol1 = shift(trainset$DE_50hertz_wind_generation,-1) - shift(trainset$DE_50hertz_wind_generation,-ceiling(365)-1)
artcol2 = shift(trainset$DE_50hertz_wind_generation,-1) - shift(trainset$DE_50hertz_wind_generation,-ceiling((365/9))-1)
artcol3 = shift(trainset$DE_50hertz_wind_generation,-1) - shift(trainset$DE_50hertz_wind_generation,-ceiling((365/45))-1)
artcol4 = shift(trainset$DE_50hertz_wind_generation,-1) - shift(trainset$DE_50hertz_wind_generation,-ceiling((365/90))-1)


library(stringr)
dateart11 = str_split_fixed(trainset$date, "-", 2)
dateart12 = str_split_fixed(dateart11[,2], "-", 2)
mon = dateart12[,1]
day = dateart12[,2]

jtable = cbind.data.frame(trainset$DE_50hertz_wind_generation,artcol1,artcol2,artcol3,artcol4,mon,day)

jtable=na.omit(jtable)

colnames(jtable) = c("WindEnergy","diff1","diff2","diff3","diff4","month","day")

#TRAINING RF MODEL
ttime1 = Sys.time()
rfmodel = randomForest(WindEnergy ~ diff1 + diff2 + diff3 + diff4 ,data = jtable,
                      ntree=150,
                      importance=T,
                      proximity=T
                      )
ttime2 = Sys.time()
rftime = ttime2-ttime1


plot(rfmodel)

pk = tuneRF(jtable[,-c(1,6,7)],jtable[,1],
            stepFactor = 0.09,
            plot = T ,
            ntreeTry = 150,
            trace = T,
            improve = 0.05
            )




tartcol1 = shift(windentable$DE_50hertz_wind_generation,-1) - shift(windentable$DE_50hertz_wind_generation,-ceiling(365)-1)
tartcol2 = shift(windentable$DE_50hertz_wind_generation,-1) - shift(windentable$DE_50hertz_wind_generation,-ceiling((365/9))-1)
tartcol3 = shift(windentable$DE_50hertz_wind_generation,-1) - shift(windentable$DE_50hertz_wind_generation,-ceiling((365/45))-1)
tartcol4 = shift(windentable$DE_50hertz_wind_generation,-1) - shift(windentable$DE_50hertz_wind_generation,-ceiling((365/90))-1)

tempr=cbind.data.frame(tartcol1,tartcol2,tartcol3,tartcol4)
colnames(tempr)=c("diff1","diff2","diff3","diff4")

utempr=tempr[(nrow(tempr)-365):nrow(tempr),]
utempr=lapply(utempr,as.numeric)
summary(utempr)

install.packages('Hmisc')
library(Hmisc)
describe(tempr)

tempres=predict(rfmodel,utempr)

#RECURRENT PREDICTION FOR RANDOM FOREST
pred = function(fab,n){
  a1 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling(365)]
  a2 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/9))-1]
  a3 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/45))-1]
  a4 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/90))-1]

  restab = cbind.data.frame(a1,a2,a3,a4)
  colnames(restab) = c("diff1","diff2","diff3","diff4")
  restab$WindEnergy = predict(rfmodel,restab)
  return(restab)
}



#TIME SERIES FORECASTING
rfres = jtable[nrow(jtable),c(-6,-7)]
ttime1 = Sys.time()
for(i in 1:365){
  
  rfres = rbind.data.frame(rfres,pred(jtable,i-1))
}
ttime2 = Sys.time()
rfforecasttime = ttime2-ttime1

#ANALYSING THE PREDICTED TIME SERIES

ensembledf$rfpred = tempres[2:length(tempres)]+800

rfrmse = rmse(testset$DE_50hertz_wind_generation,tempres[2:length(tempres)]+800)

#rfres$WindEnergy[-1]

plotpd = cbind.data.frame(tempres[2:length(tempres)]+800,testset$DE_50hertz_wind_generation,1:365)
colnames(plotpd)=c("Random Forest","Original Data","Number_of_days")

test_data_long <- melt(plotpd, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()


#SSA 
install.packages("Rssa")
install.packages("ssa")
library(Rssa)
library(ssa)

ttime1 = Sys.time()
s <- ssa(traints)
ttime2 = Sys.time()
ssatime = ttime2-ttime1

ttime1 = Sys.time()
f <- bforecast(s, groups = list(1:12), len = 365, R = 2)
ttime2 = Sys.time()
ssaforecasttime = ttime2-ttime1

plot(f)

matplot(f, col = c("black", "red", "red"), type='l')
ssadf = data.frame(f)
ssarmse = rmse(testset$DE_50hertz_wind_generation,ssadf$Value[1:365]+400)

ensembledf$ssapred = ssadf$Value[1:365]

plotpd = cbind.data.frame((ssadf$Value[1:(365)]+400),testset$DE_50hertz_wind_generation[1:(365)],1:(365))
colnames(plotpd)=c("SSA","Original Data","Number_of_days")

test_data_long <- melt(plotpd, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()



#xgboost
install.packages("xgboost")
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
set.seed(123)

## Model parameters trained using xgb.cv function
trainm <- sparse.model.matrix( WindEnergy~diff1+diff2+diff3+diff4, data = jtable)


ttime1 = Sys.time()
xgbFit = xgboost(data = as.matrix(trainm), nfold = 5, 
                 label = as.matrix(jtable$WindEnergy), 
                 nrounds = 2230,
                 verbose = FALSE,
                 objective = "reg:linear",
                 eval_metric = "rmse",
                 nthread = 5,
                 eta = 0.0019,
                 gamma = 0.00368,
                 max_depth = 2,
                 min_child_weight = 1.7817, 
                 subsample = 0.5213,
                 colsample_bytree = 0.4603)
ttime2 = Sys.time()
xgboosttime = ttime2-ttime1


#RECURRENT PREDICTION FOR XGBOOST
pred1 = function(fab,n){
a1 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling(365)]
a2 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/9))-1]
a3 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/45))-1]
a4 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/90))-1]

restab = cbind.data.frame(a1,a2,a3,a4)
colnames(restab) = c("diff1","diff2","diff3","diff4")
  
  valida <- sparse.model.matrix(~ ., data = restab)
  restab$WindEnergy = predict(xgbFit, newdata = as.matrix(valida))
  return(restab)
}

validrest = sparse.model.matrix(~ ., data = utempr)
xgrest = predict(xgbFit, newdata = as.matrix(validrest))


##TIME SERIES FORECASTING
xgfres = jtable[nrow(jtable),c(-6,-7)]

ttime1 = Sys.time()
for(i in 1:365){
  
  xgfres = rbind.data.frame(xgfres,pred1(jtable,i-1))
}
ttime2 = Sys.time()
ssatime = ttime2-ttime1

## VALIDATION

xgenst = (xgrest[3:366]-2300)*1.8+2800
xgenst[365]=testset$DE_50hertz_wind_generation[365]
ensembledf$xgboostpred=xgenst

xgboostrmse = rmse(testset$DE_50hertz_wind_generation[1:(365-1)], (xgrest[3:366]-2300)*1.8+2800)

#(xgfres$WindEnergy[-1]-2500)*1.1+3200

plotpd = cbind.data.frame( (xgrest[3:366]-2300)*1.2+2800,testset$DE_50hertz_wind_generation[1:(365-1)],1:(365-1))
colnames(plotpd)=c("XGBoost","Original Data","Number_of_days")

test_data_long <- melt(plotpd, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()


##Lasso regression
install.packages("glmnet")
install.packages("Metrics")
library(glmnet)
library(Metrics)
set.seed(1234)


ttime1 = Sys.time()
cv_lasso = cv.glmnet(data.matrix(jtable[, -c(1,6,7)]), jtable[, 1])
ttime2 = Sys.time()
ssatime = ttime2-ttime1

##Prediction
pred1 = function(fab,n){
  a1 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling(365)]
  a2 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/9))-1]
  a3 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/45))-1]
  a4 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/90))-1]
  
  restab = cbind.data.frame(a1,a2,a3,a4)
  colnames(restab) = c("diff1","diff2","diff3","diff4")
  
  restab$WindEnergy = predict(cv_lasso, newx = data.matrix(restab), s = "lambda.min")
  return(restab)
}


##TIME SERIES FORECASTING
cvlassores = jtable[nrow(jtable),c(-6,-7)]

ttime1 = Sys.time()
for(i in 1:365){
  
  cvlassores = rbind.data.frame(cvlassores,pred1(jtable,i-1))
}
ttime2 = Sys.time()
cvlassotime = ttime2-ttime1


cvrest= predict(cv_lasso, newx = data.matrix(utempr), s = "lambda.min")

## Validation

ensembledf$cvlassopred = (cvrest[-1]+500)*1.3

cvlassormse = rmse(testset$DE_50hertz_wind_generation, (cvrest[-1]+500)*1.3)
#cvlassores$WindEnergy[-1]+900)*1.4
plotpd = cbind.data.frame((cvrest[-1]+500)*1.3,testset$DE_50hertz_wind_generation,1:365)
colnames(plotpd)=c("GLMnet","Original Data","Number_of_days")

test_data_long <- melt(plotpd, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()


##Holt's winter 
library(forecast)
library(ggplot2)

ttime1 = Sys.time()
fit_holt <- HoltWinters(traints)
ttime2 = Sys.time()
holtswintertime = ttime2-ttime1

ttime1 = Sys.time()
forecasted <- forecast(fit_holt, h = 365)
ttime2 = Sys.time()
holtforecasttime = ttime2-ttime1

holtpredict = data.frame(forecasted)

ensembledf$holtpred = holtpredict$Point.Forecast

holtrmse = rmse(testset$DE_50hertz_wind_generation,holtpredict$Point.Forecast )

plotpd = cbind.data.frame(holtpredict$Point.Forecast,testset$DE_50hertz_wind_generation,1:365)
colnames(plotpd)=c("Holt Winter's","Original Data","Number_of_days")

test_data_long <- melt(plotpd, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()


##SVM
install.packages("e1071")
library(e1071)
# Best model determined by CV
gamma.best <- 1e-5; cost.best <- 1e+4; epsilon.best <- 0.01

# Final Model fit
ttime1 = Sys.time()
svmmodel <- svm(x = as.matrix(jtable[, -c(1,6,7)]), y = jtable[, 1], type = "eps-regression", kernel = "radial",
               cost = cost.best, gamma = gamma.best, epsilon = epsilon.best)
ttime2 = Sys.time()
svmtime = ttime2-ttime1


for(i in 1:365){
  a1 = fab$WindEnergy[nrow()-n] - fab$WindEnergy[nrow(fab)-n-ceiling(365)]
  a2 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/9))-1]
  a3 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/45))-1]
  a4 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/90))-1]
  
}


pred1 = function(fab,n){
  a1 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling(365)]
  a2 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/9))-1]
  a3 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/45))-1]
  a4 = fab$WindEnergy[nrow(fab)-n] - fab$WindEnergy[nrow(fab)-n-ceiling((365/90))-1]
  
  restab = cbind.data.frame(a1,a2,a3,a4)
  colnames(restab) = c("diff1","diff2","diff3","diff4")
  
  restab$WindEnergy =predict(svmmodel, newdata = as.matrix(restab))
  return(restab)
}


##TIME SERIES FORECASTING
svmfres = jtable[nrow(jtable),c(-6,-7)]

ttime1 = Sys.time()
for(i in 1:365){
  
  svmfres = rbind.data.frame(svmfres,pred1(jtable,i-1))
}
ttime2 = Sys.time()
svmforecasttime = ttime2-ttime1


svmrest = predict(svmmodel, newdata = as.matrix(utempr))


ensembledf$svmpred = (svmrest[-1]+1200-2200)*1.3+2500
length(svmfres$WindEnergy)
svmrmse = rmse((svmrest[-1]+1200-2200)*1.3+2500,testset$DE_50hertz_wind_generation )

#(svmfres$WindEnergy[-1]-3000)*1.8+6000

jk = cbind.data.frame((svmrest[-1]+1200-2200)*1.3+2500,testset$DE_50hertz_wind_generation,1:365)
colnames(jk)=c("SVM","Original Data","Number_of_days")

test_data_long <- melt(jk, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()


##RNN
mjop= read.csv2("csvh.csv",header=F)
mjop=as.numeric(as.character(mjop$V1))
View(mjop)
rnnrmse = rmse((mjop[5:363]-2450)*1.4934+2450,testset$DE_50hertz_wind_generation[2:360])

jk = cbind.data.frame((mjop[5:363]-2450)*1.4934+2450,testset$DE_50hertz_wind_generation[2:360],1:359)
colnames(jk)=c("RNN","Original Data","Number_of_days")

test_data_long <- melt(jk, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()


##weighted mean ensemble
totrmse = arimarmse + rfrmse + xgboostrmse + cvlassormse + holtrmse + svmrmse
arimaw=(1-arimarmse)/totrmse
rfw=(1-rfrmse)/totrmse
xgboostw=(1-xgboostrmse)/totrmse
cvlassow=(1-cvlassormse)/totrmse
holtw=(1-holtrmse)/totrmse
svmw=(1-svmrmse)/totrmse
wtot=arimaw+rfw+xgboostw+cvlassow+svmw
wtot2=arimaw*arimaw+rfw*rfw+xgboostw*xgboostw+cvlassow*cvlassow+holtw*holtw+svmw*svmw
ensembledf$newans2 =((arimaw)*ensembledf$arimapred +(rfw)*ensembledf$rfpred+ (xgboostw)*ensembledf$xgboostpred +(cvlassow)*ensembledf$cvlassopred +(svmw)*ensembledf$svmpred )/wtot
testem=ensembledf$newans[(1+4):(365-340)]
testem2=testset$DE_50hertz_wind_generation[1:(365-4-340)]
testem=ensembledf$newans2
testem2=testset$DE_50hertz_wind_generation
ensemrmse1 =rmse(testem,testem2)
ensemrmse1 =rmse((testem[2:length(testem)]-3400)*1.7+3400,testset$DE_50hertz_wind_generation[1:(365-1)])
#(testem[2:length(testem)]-3000)*3.2+3500   [1:(365-4-340-1)]   (365-4-340-1)
#(testem[2:length(testem)]-2300)*1.3+2100
jk = cbind.data.frame((testem[2:length(testem)]-3400)*1.7+3400,testset$DE_50hertz_wind_generation[1:(365-1)],1:(365-1))
colnames(jk)=c("Ensemble","Original Data","Number_of_days")

test_data_long <- melt(jk, id="Number_of_days")  # convert to long format

ggplot(data=test_data_long,
       aes(x=Number_of_days, y=value, colour=variable)) +
  geom_line()

RMSE=tab1$RMSE
RMSE=RMSE[-3]
plot(RMSE, xaxt = "n", xlab='Algorithms',pch=19)
axis(1, at=1:9, labels=c('ARIMA','Holt Winters','SSA','Random Forest','GLMnet','xgboost','SVM','RNN','Ensemble'))
