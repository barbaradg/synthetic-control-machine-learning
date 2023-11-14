#install_github(repo="ryantibs/conformal", subdir="conformalInference")
#library(conformalInference)
library(Synth)
#library(devtools)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(repr)
library(ggplot2)
library(reshape2)
library(MLmetrics)
library(dplyr)
library(corrplot)

data <- read.csv("DF_SYNTH2.csv")
data<-data[data$Semana<=10,]  #seleccion de las primeras 10 semanas
data$rut_seller<-as.character(data$rut_seller)
rut<-data[data$Semana==10,]$rut_seller
data<-as.data.frame(data[(data$rut_seller %in% rut),])
rownames(data) <- 1:nrow(data)
borrar<-c('plata', 'fecha','X')
data2<-data[ ,!(names(data) %in% borrar)]
data2$suma[data2$suma==0]<-0.1
data2[3:4]<-log(data2[3:4]+1)
data2[6:14]<-log(data2[6:14]+1)

data2<-data2[c('index','rut_seller','suma','Semana','plata2','precio_desv','precio_median','precio_min','precio_75','SKU','tramo_6.0','tramo_9.0','G02','G06','G07','G09','G13','G16','G18','G21','G22','G0801','G0802','G1604','G1711','G1901','G2104','Otra.cat','X464100.0','X471990.0','X477399.0','X702000.0','X731001.0','cluster')]

train_sellers<-read.csv("train_sellers.csv")
test_sellers<-read.csv("test_sellers.csv")
train<-subset(data2,data2$index %in% train_sellers$seller)
test<-subset(data2,data2$index %in% test_sellers$seller)
train['index']<-rep(1:643,each=10)

index_train<-as.numeric(unique(train$index))
index_test<-as.numeric(unique(test$index))

MAPE_TRAIN<-c()
MAPE_TEST<-c()
WMAPE_TRAIN<-c()
WMAPE_TEST<-c()
RMSE_TRAIN<-c()
RMSE_TEST<-c()
pred_train<-c()
real_train<-c()
seller<-c()
sels_train<-c()
#SET SELLERS TRAIN
for (i in (index_train)){
  tryCatch({
    print(i)
    data_400.out <-
      dataprep(
        foo = train,
        predictors    = "plata2",
        dependent     = "suma",
        unit.variable = "index",
        time.variable = 4,
        special.predictors = list(
          list( "suma",1,c('mean')),
          list( "suma",2,c('mean')),
          list( "suma",3,c('mean')),
          list( "suma",4,c('mean')),
          list( "SKU",1, c("mean")),
          list( "precio_desv",1,c("mean")),
          list( "precio_median",1,c("mean")),
          list( "precio_min",1,c("mean")),
          list( "precio_75",1,c("mean")),
          list( "tramo_6.0",1,c("mean")),
          list( "tramo_9.0",1,c("mean")),
          list( "G02",1,c("mean")),
          list( "G06",1,c("mean")),
          list( "G07",1,c("mean")),
          list( "G09",1,c("mean")),
          list( "G16",1,c("mean")),
          list( "G18",1,c("mean")),
          list( "G21",1,c("mean")),
          list( "G22",1,c("mean")),
          list( "G0801",1,c("mean")),
          list( "G0802",1,c("mean")),
          list( "G1604",1,c("mean")),
          list( "G1711",1,c("mean")),
          list( "G1901",1,c("mean")),
          list( "G2104",1,c("mean")),
          list( "Otra.cat",1,c("mean")),
          list( "X464100.0",1,c("mean")),
          list( "X471990.0",1,c("mean")),
          list( "X477399.0",1,c("mean")),
          list( "X731001.0",1,c("mean"))
        ),
        treatment.identifier = i,
        controls.identifier = index_train[-i],
        time.predictors.prior = 1:4,
        time.optimize.ssr = 1:4,
        unit.names.variable = 2,
        time.plot = 1:10
      )
    resto<-rep(0.2/21,21)
    synth_400.out<-synth(data_400.out,custom.v=c(0.05,0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,resto))
 
    #Errores
    predicted_train<-exp((data_400.out$Y0plot %*% synth_400.out$solution.w)[5:10])-1
    true_train<-exp(data_400.out$Y1plot[5:10])-1
    
    predicted<-c(predicted_train)
    true<-c(true_train)
    
    MAPE_train<- MAPE(predicted_train,true_train)*100
    WMAPE_train<-(sum(abs(true_train-predicted_train))/sum(true_train))*100
    RMSE_train<-RMSE(predicted_train, true_train)
    
    df_errores<-data.frame(
      MAPE_train=MAPE_train,
      WMAPE_train=WMAPE_train,
      RMSE_train=RMSE_train
    )
    print(df_errores)
    
    MAPE_TRAIN<-c(MAPE_TRAIN,MAPE_train)
    WMAPE_TRAIN<-c(WMAPE_TRAIN,WMAPE_train)
    RMSE_TRAIN<-c(RMSE_TRAIN, RMSE_train)
    pred_train<-c(pred_train, predicted)
    real_train<-c(real_train, true)
    seller<-c(seller,i)
    sel<-rep(i,6)
    sels_train<-c(sels_train,sel)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
#SET SELLERS TESTEO
train<-subset(data2,data2$index %in% train_sellers$seller)
index_train<-as.numeric(unique(train$index))
pred_test<-c()
real_test<-c()
sels_test<-c()
for (i in (index_test)){
  tryCatch({
    print(i)
    data_400.out <-
      dataprep(
        foo = data2,
        predictors    = "plata2",
        dependent     = "suma",
        unit.variable = "index",
        time.variable = 4,
        special.predictors = list(
          list( "suma",1,c('mean')),
          list( "suma",2,c('mean')),
          list( "suma",3,c('mean')),
          list( "suma",4,c('mean')),
          list( "SKU",1, c("mean")),
          list( "precio_desv",1,c("mean")),
          list( "precio_median",1,c("mean")),
          list( "precio_min",1,c("mean")),
          list( "precio_75",1,c("mean")),
          list( "tramo_6.0",1,c("mean")),
          list( "tramo_9.0",1,c("mean")),
          list( "G02",1,c("mean")),
          list( "G06",1,c("mean")),
          list( "G07",1,c("mean")),
          list( "G09",1,c("mean")),
          list( "G16",1,c("mean")),
          list( "G18",1,c("mean")),
          list( "G21",1,c("mean")),
          list( "G22",1,c("mean")),
          list( "G0801",1,c("mean")),
          list( "G0802",1,c("mean")),
          list( "G1604",1,c("mean")),
          list( "G1711",1,c("mean")),
          list( "G1901",1,c("mean")),
          list( "G2104",1,c("mean")),
          list( "Otra.cat",1,c("mean")),
          list( "X464100.0",1,c("mean")),
          list( "X477399.0",1,c("mean")),
          list( "X731001.0",1,c("mean"))
        ),
        treatment.identifier = i,
        controls.identifier = index_train,
        time.predictors.prior = 1:4,
        time.optimize.ssr = 1:4,
        unit.names.variable = 2,
        time.plot = 1:10
      )
    resto<-rep(0.2/21,21)
    synth_400.out<-synth(data_400.out,custom.v=c(0.05,0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,resto))
    
    #Errores
    
    predicted_test<-exp((data_400.out$Y0plot %*% synth_400.out$solution.w)[5:10])-1
    true_test<-exp(data_400.out$Y1plot[5:10])-1
    
    predicted<-c(predicted_test)
    true<-c(true_test)

    MAPE_test<- MAPE(predicted_test,true_test)*100
    WMAPE_test<-(sum(abs(true_test-predicted_test))/sum(true_test))*100
    RMSE_test<-RMSE(predicted_test, true_test)
    
    df_errores<-data.frame(
      MAPE_test=MAPE_test,
      WMAPE_test=WMAPE_test,
      RMSE_test=RMSE_test
    )
    print(df_errores)

    MAPE_TEST<-c(MAPE_TEST,MAPE_test)
    WMAPE_TEST<-c(WMAPE_TEST,WMAPE_test)
    RMSE_TEST<-c(RMSE_TEST, RMSE_test)
    pred_test<-c(pred_test, predicted)
    real_test<-c(real_test, true)
    seller<-c(seller,i)
    sel_test<-rep(i,6)
    sels_test<-c(sels_test,sel_test)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
#Factor Duan 
factor<-duan_smearing_factor(real_train,pred_train)
train_pred_adj<-(factor*pred_train)
df_adj_train<-as.data.frame(train_pred_adj)
df_adj_train$pred<-pred_train
df_adj_train$real<-real_train
df_adj_train$index<-sels_train

factor<-duan_smearing_factor(real_test,pred_test)
test_pred_adj<-(factor*pred_test)
df_adj_test<-as.data.frame(test_pred_adj)
df_adj_test$pred<-pred_test
df_adj_test$real<-real_test
df_adj_test$index<-sels_test

sels_test<-rep(seller,each=6)

predicciones<-data.frame(
  "seller"=rep(0,length(sels)),
  "Semana"=rep(1:10,length(unique(sels))),
  "prediccion"=rep(0,length(sels)),
  "real"=rep(0,length(sels)),
  "resta"=rep(0,length(sels))
  
)
length(sels)
predicciones['seller']<-sels[2465:6160]
predicciones['prediccion']<-df_adj$pred_adj
predicciones['real']<-real

predicciones['resta']<- predicciones$prediccion-predicciones$real
pred_ent<-predicciones[predicciones$Semana<=4,]
pred_test<-predicciones[predicciones$Semana>4,]

MAPE_ENT_ADJ<-c()
WMAPE_ENT_ADJ<-c()
RMSE_ENT_ADJ<-c()
MAPE_TEST_ADJ<-c()
WMAPE_TEST_ADJ<-c()
RMSE_TEST_ADJ<-c()
dim(df_adj_test)
3768/6
df_adj_train$Semana<-rep(5:10,628)
942/6
df_adj_test$Semana<-rep(5:10,157)

ent<-df_adj_train
test<-df_adj_test
for (i in unique(sels_train)){
  PRED=ent[ent$index==i,'train_pred_adj']
  REAL=ent[ent$index==i,'real']
  MAPE_ENT_ADJ<-c(MAPE_ENT_ADJ, MAPE(PRED,REAL)*100)
  RMSE_ENT_ADJ<- c(RMSE_ENT_ADJ,RMSE(PRED,REAL))
  WMAPE_ENT_ADJ<-c(WMAPE_ENT_ADJ,(sum(abs(REAL-PRED))/sum(REAL))*100)
}

for (i in unique(sels_test)){
  PRED=test[test$index==i,'test_pred_adj']
  REAL=test[test$index==i,'real']
  MAPE_TEST_ADJ<-c(MAPE_TEST_ADJ, MAPE(PRED,REAL)*100)
  RMSE_TEST_ADJ<- c(RMSE_TEST_ADJ,RMSE(PRED,REAL))
  WMAPE_TEST_ADJ<-c(WMAPE_TEST_ADJ,(sum(abs(REAL-PRED))/sum(REAL))*100)
}

df_error_train<-as.data.frame(cbind(MAPE_ENT_ADJ,WMAPE_ENT_ADJ,RMSE_ENT_ADJ))
summary(df_error_train)
df_error_test<-as.data.frame(cbind(MAPE_TEST_ADJ,WMAPE_TEST_ADJ,RMSE_TEST_ADJ))
summary(df_error_test)
