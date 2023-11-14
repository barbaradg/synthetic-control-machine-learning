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

duan_smearing_factor<-function(y,y_pred){
  n=length(y)
  resid=y-y_pred
  sigma=sd(resid)
  smearing_factor= exp((1/n)* sum(resid^2)/(2*sigma^2))-((n-1)/(2*n))
  return(smearing_factor)
}
data <- read.csv("DF_SYNTH2_km.csv")
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
          #list( "precio_prom",1, c('mean')),
          list( "SKU",1, c("mean")),
          list( "precio_desv",1,c("mean")),
          list( "precio_median",1,c("mean")),
          #list( "precio_max",1,c("mean")),
          list( "precio_min",1,c("mean")),
          #list( "precio_25",1,c("mean")),
          list( "precio_75",1,c("mean")),
          list( "cluster",1,c("mean")),
          #list( "tramo_1.0",1,c("mean")),
          #list( "tramo_2.0",1,c("mean")),
          #list( "tramo_3.0",1,c("mean")),
          #list( "tramo_4.0",1,c("mean")),
          #list( "tramo_5.0",1,c("mean")),
          list( "tramo_6.0",1,c("mean")),
          #list( "tramo_7.0",1,c("mean")),
          #list( "tramo_8.0",1,c("mean")),
          list( "tramo_9.0",1,c("mean")),
          #list( "tramo_10.0",1,c("mean")),
          # list( "tramo_11.0",1,c("mean")),
          # list( "tramo_12.0",1,c("mean")),
          # list( "tramo_13.0",1,c("mean")),
          #list( "G01",1,c("mean")),
          list( "G02",1,c("mean")),
          #list( "G03",1,c("mean")),
          #list( "G04",1,c("mean")),
          #list( "G05",1,c("mean")),
          list( "G06",1,c("mean")),
          list( "G07",1,c("mean")),
          #list( "G08",1,c("mean")),
          list( "G09",1,c("mean")),
          #list( "G10",1,c("mean")),
          #list( "G11",1,c("mean")),
          #list( "G12",1,c("mean")),
          #list( "G13",1,c("mean")),
          #list( "G14",1,c("mean")),
          list( "G16",1,c("mean")),
          #list( "G17",1,c("mean")),
          list( "G18",1,c("mean")),
          #list( "G19",1,c("mean")),
          #list( "G20",1,c("mean")),
          list( "G21",1,c("mean")),
          list( "G22",1,c("mean")),
          list( "G0801",1,c("mean")),
          list( "G0802",1,c("mean")),
          #list( "G1202",1,c("mean")),
          #list( "G1302",1,c("mean")),
          #list( "G1403",1,c("mean")),
          #list( "G1602",1,c("mean")),
          list( "G1604",1,c("mean")),
          #list( "G1612",1,c("mean")),
          list( "G1711",1,c("mean")),
          #list( "G1802",1,c("mean")),
          #list( "G1803",1,c("mean")),
          list( "G1901",1,c("mean")),
          #list( "G1902",1,c("mean")),
          # list( "G1903",1,c("mean")),
          # list( "G1907",1,c("mean")),
          # list( "G2101",1,c("mean")),
          # list( "G2102",1,c("mean")),
          # list( "G2103",1,c("mean")),
          list( "G2104",1,c("mean")),
          # list( "G2105",1,c("mean")),
          list( "Otra.cat",1,c("mean")),
          list( "X464100.0",1,c("mean")),
          # list( "X464902.0",1,c("mean")),
          # list( "X464903.0",1,c("mean")),
          # list( "X464909.0",1,c("mean")),
          # list( "X469000.0",1,c("mean")),
          list( "X471990.0",1,c("mean")),
          # list( "X474100.0",1,c("mean")),
          # list( "X475901.0",1,c("mean")),
          # list( "X475909.0",1,c("mean")),
          # list( "X476309.0",1,c("mean")),
          # list( "X476400.0",1,c("mean")),
          # list( "X477101.0",1,c("mean")),
          # list( "X477102.0",1,c("mean")),
          # list( "X477203.0",1,c("mean")),
          list( "X477399.0",1,c("mean")),
          # list( "X479100.0",1,c("mean")),
          # list( "X479909.0",1,c("mean")),
          # list( "X702000.0",1,c("mean")),
          list( "X731001.0",1,c("mean"))
          # list( "X829900.0",1,c("mean")),
          # list( "Otro",1,c("mean"))
        ),
        treatment.identifier = i,
        controls.identifier = index_train[-i],
        time.predictors.prior = 1:4,
        time.optimize.ssr = 1:4,
        unit.names.variable = 2,
        time.plot = 1:10
      )
    resto<-rep(0.2/21,21)
    synth_400.out<-synth(data_400.out,custom.v=c(0.05,0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.05,resto))
    
 
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
          #list( "precio_prom",1, c('mean')),
          list( "SKU",1, c("mean")),
          list( "precio_desv",1,c("mean")),
          list( "precio_median",1,c("mean")),
          #list( "precio_max",1,c("mean")),
          list( "precio_min",1,c("mean")),
          #list( "precio_25",1,c("mean")),
          list( "precio_75",1,c("mean")),
          list("cluster",1,c("mean")),
          #list( "tramo_1.0",1,c("mean")),
          #list( "tramo_2.0",1,c("mean")),
          #list( "tramo_3.0",1,c("mean")),
          #list( "tramo_4.0",1,c("mean")),
          #list( "tramo_5.0",1,c("mean")),
          list( "tramo_6.0",1,c("mean")),
          #list( "tramo_7.0",1,c("mean")),
          #list( "tramo_8.0",1,c("mean")),
          list( "tramo_9.0",1,c("mean")),
          #list( "tramo_10.0",1,c("mean")),
          # list( "tramo_11.0",1,c("mean")),
          # list( "tramo_12.0",1,c("mean")),
          # list( "tramo_13.0",1,c("mean")),
          #list( "G01",1,c("mean")),
          list( "G02",1,c("mean")),
          #list( "G03",1,c("mean")),
          #list( "G04",1,c("mean")),
          #list( "G05",1,c("mean")),
          list( "G06",1,c("mean")),
          list( "G07",1,c("mean")),
          #list( "G08",1,c("mean")),
          list( "G09",1,c("mean")),
          #list( "G10",1,c("mean")),
          #list( "G11",1,c("mean")),
          #list( "G12",1,c("mean")),
          #list( "G13",1,c("mean")),
          #list( "G14",1,c("mean")),
          list( "G16",1,c("mean")),
          #list( "G17",1,c("mean")),
          list( "G18",1,c("mean")),
          #list( "G19",1,c("mean")),
          #list( "G20",1,c("mean")),
          list( "G21",1,c("mean")),
          list( "G22",1,c("mean")),
          list( "G0801",1,c("mean")),
          list( "G0802",1,c("mean")),
          #list( "G1202",1,c("mean")),
          #list( "G1302",1,c("mean")),
          #list( "G1403",1,c("mean")),
          #list( "G1602",1,c("mean")),
          list( "G1604",1,c("mean")),
          #list( "G1612",1,c("mean")),
          list( "G1711",1,c("mean")),
          #list( "G1802",1,c("mean")),
          #list( "G1803",1,c("mean")),
          list( "G1901",1,c("mean")),
          #list( "G1902",1,c("mean")),
          # list( "G1903",1,c("mean")),
          # list( "G1907",1,c("mean")),
          # list( "G2101",1,c("mean")),
          # list( "G2102",1,c("mean")),
          # list( "G2103",1,c("mean")),
          list( "G2104",1,c("mean")),
          # list( "G2105",1,c("mean")),
          list( "Otra.cat",1,c("mean")),
          list( "X464100.0",1,c("mean")),
          # list( "X464902.0",1,c("mean")),
          # list( "X464903.0",1,c("mean")),
          # list( "X464909.0",1,c("mean")),
          # list( "X469000.0",1,c("mean")),
          list( "X471990.0",1,c("mean")),
          # list( "X474100.0",1,c("mean")),
          # list( "X475901.0",1,c("mean")),
          # list( "X475909.0",1,c("mean")),
          # list( "X476309.0",1,c("mean")),
          # list( "X476400.0",1,c("mean")),
          # list( "X477101.0",1,c("mean")),
          # list( "X477102.0",1,c("mean")),
          # list( "X477203.0",1,c("mean")),
          list( "X477399.0",1,c("mean")),
          # list( "X479100.0",1,c("mean")),
          # list( "X479909.0",1,c("mean")),
          # list( "X702000.0",1,c("mean")),
          list( "X731001.0",1,c("mean"))
          # list( "X829900.0",1,c("mean")),
          # list( "Otro",1,c("mean"))
        ),
        treatment.identifier = i,
        controls.identifier = index_train,
        time.predictors.prior = 1:4,
        time.optimize.ssr = 1:4,
        unit.names.variable = 2,
        time.plot = 1:10
      )
    
    resto<-rep(0.2/21,21)
    synth_400.out<-synth(data_400.out,custom.v=c(0.05,0.05,0.05,0.05,0.05,0.1,0.1,0.1,0.1,0.1,0.05,resto))
    
    
    
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

synth_400.out$solution.v
#796= 5.475183e-01, 659=1.121928e-01, 142= 1.030385e-01,199=  9.309337e-02,206=  8.027764e-02, 177= 2.118147e-02
# 184= 1.854200e-02, 389= 8.666630e-03, 769= 7.856988e-03 , 230= 5.395123e-03



factor<-duan_smearing_factor(real_train,pred_train)
train_pred_adj<-(factor*pred_train)
df_adj_train<-as.data.frame(train_pred_adj)
df_adj_train$pred<-pred_train
df_adj_train$real<-real_train
df_adj_train$index<-sels_train

#test
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

print(paste('Porcentaje subestimado modelo control sintetico set entrenamiento:',round(dim(pred_ent[pred_ent$resta<0,])[1]/dim(pred_ent)[1],3)*100,'%'))
print(paste('Porcentaje subestimado modelo control sintetico set testeo:',round(dim(pred_test[pred_test$resta<0,])[1]/dim(pred_test)[1],3)*100,'%'))
print(paste('Porcentaje sobreestimado modelo control sintetico set entrenamiento:',round(dim(pred_ent[pred_ent$resta>0,])[1]/dim(pred_ent)[1],3)*100,'%'))
print(paste('Porcentaje sobreestimado modelo control sintetico set testeo:',round(dim(pred_test[pred_test$resta>0,])[1]/dim(pred_test)[1],3)*100,'%'))

print(paste('Promedio error entrenamiento:', mean(pred_ent$resta)))
print(paste('Promedio error testeo:', mean(pred_test$resta)))
MAPE_ENT_ADJ<-c()
WMAPE_ENT_ADJ<-c()
RMSE_ENT_ADJ<-c()


#test
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


write.csv(df_error_test,'cs.csv')
